{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Smutt.HTTP.Server.Request.Reader (
   readHTTP
  ) where


import Control.Monad.Except

import Smutt.HTTP.Method as Method
import Smutt.HTTP.Version as Version

import Smutt.HTTP.Header as Header
import Smutt.HTTP.Header.Reader as Header
import Smutt.HTTP.Header.RequestLine

import Smutt.HTTP.Header.RequestLine as ReqLine

import Smutt.HTTP.Server.Settings
import Network.BufferedSocket (BufferedSocket)
import qualified Network.BufferedSocket as BS

import qualified Smutt.HTTP.Server.Request.Content.Raw.Chunked as Chunked
import qualified Smutt.HTTP.Server.Request.Content.Raw.Exact as Exact
import Smutt.HTTP.Server.Request 

import Smutt.Util.ByteString
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as B

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T 


import Data.Maybe

import qualified Data.HashMap.Strict as HM

import Control.Concurrent.MVar

import Smutt.HTTP.Server.Request.Content.Error
import Smutt.HTTP.Error




-- | Reads the request headers from the current socket in case the request is malformed this returns an error 
readHTTP :: BufferedSocket -> ServerSettings -> ExceptT ReaderError IO Request
readHTTP bSocket settings = do
  requestLineStr <- ExceptT $ maybe (Left HeaderTooLarge) Right <$> BS.readToSequenceLimited bSocket crlf (maxRequestLineLength settings)
  liftIO $ print requestLineStr
  reqLine@(RequestLine _ _ _ version) <- ExceptT $ return $  ReqLine.fromString requestLineStr
  liftIO $ print reqLine
  headers <- Header.read (maxHeaderLineLength settings) (maxHeaderLength settings) bSocket
  
  -- Diffent http versions should be handled in diffrent ways
  -- Thus here we split it up into diffrent functions to 
  case version of
    (HTTP 1 1) -> http11Reader bSocket settings reqLine headers
    (HTTP 1 0) -> http10Reader bSocket settings reqLine headers
    _ -> throwError HTTPVersionNotSupported


-- | reads HTTP/1.1 requests
http11Reader :: BufferedSocket -> ServerSettings -> RequestLine ByteString -> Header -> ExceptT ReaderError IO Request
http11Reader bSocket settings (RequestLine method urlRaw url version) headers = do  
  
  -- HTTP 1.1 servers should send a "100 continue" request
  liftIO $ send100Continue bSocket
  liftIO $ BS.flush bSocket

  -- HTTP 1.1 request MUST have a Host header
  liftIO $ print headers
  when (isNothing (Header.lookup "host" headers)) $
    throwError $ BadRequest "Missing Host"

  let
    maybeContentLength  = getContentLength headers
    hasContentLength    = isJust maybeContentLength
    hasTransferEncoding = Header.member "transfer-encoding" headers
    
  -- HTTP 1.1 requests that sends either POST or PUT requests MUST have  either the Transfer-Coding header or Content-Length header
  when ((POST == method || method == PUT) && not (hasContentLength || hasTransferEncoding)) 
        (throwError $ BadRequest "Missing message transfer data") 
 
  let
    readBase = case maybeContentLength >>= Just . ExactBase of
      Nothing -> if hasTransferEncoding then Just ChunkedBase else Nothing
      a -> a

    bodyType = case readBase of
      Nothing -> Nothing
      Just (ExactBase _) -> Just Exact
      Just ChunkedBase -> Just Chunked

  (maybeRSMVar, readAll, readPart) <- liftIO $ makeReadStateAndReaders bSocket readBase

    
  return $  Request method urlRaw url version headers maybeContentLength bodyType bSocket settings maybeRSMVar readAll readPart
  


-- | reads HTTP/1.0 requests 
http10Reader :: BufferedSocket -> ServerSettings -> RequestLine ByteString -> Header -> ExceptT ReaderError IO Request
http10Reader bSocket settings (RequestLine method urlRaw url version) headers = do  

  when ((method == PUT || method == POST ) && not (Header.member "content-length" headers)) $
    throwError $ BadRequest "HTTP/1.0 Requests Must have a Content-Length"
  
  let
    contentLength = getContentLength headers
    bodyType =  contentLength >>= Just . const Exact
    readBase = contentLength >>= Just . ExactBase 

  (maybeRSMVar, readAll, readPart) <- liftIO $ makeReadStateAndReaders bSocket readBase

  let request = Request method urlRaw url version headers contentLength bodyType bSocket settings maybeRSMVar readAll readPart
  
  return request

  



-- UTIL FUNCTIONS BELLOW HERE

-- | Used to bind the basic binding functions
data ReadStateBase = ExactBase Int | ChunkedBase 



readHeader :: BufferedSocket -> ServerSettings ->  ExceptT ReaderError IO Header
readHeader bSocket settings = Header.read (maxHeaderLineLength settings) (maxHeaderLength settings) bSocket


send100Continue :: BufferedSocket -> IO ()
send100Continue bSocket = BS.send bSocket message >> BS.flush bSocket
  where 
    message = "HTTP/1.1 100 continue\r\n\r\n" :: B.ByteString

getContentLength :: Header -> Maybe Int
getContentLength headers = Header.lookup "content-length" headers >>= readInt
  where
    readInt :: T.Text -> Maybe Int
    readInt = (either (const Nothing) (Just  . fst)) .  (T.decimal :: T.Reader Int)



-- This function returns the elementary reading operations for a request
makeReadStateAndReaders :: BufferedSocket -> Maybe ReadStateBase -> IO (Maybe (MVar ReadState) , IO (Either ContentError ByteString), Int -> IO (Either ContentError (Int, ByteString)))
-- I figured attempting to read from a non existing body is the same as EOF 
makeReadStateAndReaders _ Nothing =  return (Nothing, return (Left EOF), \ _ -> return (Left EOF))
makeReadStateAndReaders bSock (Just (ExactBase length)) = do
  mvar <- liftIO $ newMVar (ExactState length)
  return (Just mvar,
          runExceptT $ Exact.readContents bSock mvar,
          runExceptT . Exact.readLength bSock mvar)

-- The chunked state needs the bytes in the primary chunk read
makeReadeStateAndReader bSock (Just ChunkedBase) = do
  readState <- Chunked.readChunkedHead bSock blankReadState
  mvar <- liftIO $ newMVar readState
  return (Just mvar, Chunked.readAll bSock mvar, Chunked.readPart bSock mvar)
  where
    blankReadState = ChunkedState 0 0 False Nothing 
