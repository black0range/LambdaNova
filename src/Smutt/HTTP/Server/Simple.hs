{-# LANGUAGE OverloadedStrings  #-}
module Smutt.HTTP.Server.Simple (
  module X,
  simpleHTTP
  ) where


import Smutt.HTTP.StatusCodes as StatusCode

import Smutt.HTTP.Server.Settings as X 
import Smutt.HTTP.Server.Request as X 
import Smutt.HTTP.Server.Response as X

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as T


import Smutt.HTTP.Server.Request
import Smutt.HTTP.Header
import Smutt.HTTP.Server.Request.Reader 
import Smutt.HTTP.Error 

import Smutt.Util.TCPServer
import Smutt.Util.ByteString
import qualified Smutt.Util.URL.Simple as URL

import Network.BufferedSocket (BufferedSocket)
import qualified Network.BufferedSocket as BS

import Control.Monad.Except

import Data.Monoid
import Data.Either
import Data.String as Native
import Data.Maybe
import Smutt.HTTP.Version as Version

import System.IO.Error


type WebThunk = (Request -> IO Response)
-- | A simple HTTP server 
simpleHTTP :: ServerSettings -> WebThunk -> IO ()
simpleHTTP settings webThunk = do
  makeTCPServer (httpThunk settings webThunk) settings
 



-- Main connection handler 
httpThunk :: ServerSettings -> WebThunk -> BufferedSocket -> IO ()
httpThunk settings webThunk bSock = do
  eitherRequest <- runExceptT $ readHTTP bSock settings
  print eitherRequest 
  case eitherRequest of
    Left error -> responseHandler bSock (blankRequest settings bSock) (criticalErrorResponse error) >> BS.flush bSock
    Right request -> do
      response <- webThunk request `catchIOError` (\_ -> return $ RawResponse 500 [("Connection", "close")] "Internal server error")
      print response
      responseHandler bSock request response
      BS.flush bSock
      goOn <- shouldGoOn request response
      if goOn && isRight eitherRequest
        then httpThunk settings webThunk bSock
        else return ()
 
-- Deterines in the connection should be kept open or not
shouldGoOn :: Request -> Response -> IO Bool
shouldGoOn Request{version = HTTP 1 0} _ = return False
shouldGoOn req resp
  | hasConnectionClose req = return False 
  | responseHasClose resp = return False
  | otherwise  = reachedEOF req
     

 

closeHeader = [("Connection","close")]

criticalErrorResponse :: ReaderError -> Response
criticalErrorResponse HeaderTooLarge = RawResponse 431 closeHeader "<h5> ERROR: Header fields too large</h5>"
criticalErrorResponse (BadRequest msg) = RawResponse 400 closeHeader ("<h5> ERROR: " <> Native.fromString msg <> "</h5>")
criticalErrorResponse HTTPVersionNotSupported = RawResponse 505 closeHeader "<h5>Error: HTTP version not suported</h5>"
criticalErrorResponse LengthRequired = RawResponse 411 closeHeader "<h5>Error: Request requires a content-length</h5>"
criticalErrorResponse (NotImplemented msg) = RawResponse 501 closeHeader ("<h5> ERROR: Not implementetd " <> Native.fromString msg <> "</h5>")
criticalErrorResponse BodyTooLarge = RawResponse 413 closeHeader ("<h5> ERROR: Request body too large </h5>")





responseHandler :: BufferedSocket -> Request -> Response -> IO ()
responseHandler _ _ Manual = return ()
responseHandler bSock request (RawResponse statusCode hdr body) = do

  sendHeader bSock (version request) statusCode fullHeader
  BS.send bSock body
  where
    fullHeader = if B.null body
                 then hdr
                 else ("Content-Length", Native.fromString (show $ B.length body)) : hdr -- remove from string hack



sendHeader :: BufferedSocket -> Version -> StatusCode -> [(B.ByteString,B.ByteString)] -> IO ()
sendHeader bSock version statusCode hdrs = sendStatusLine bSock version statusCode >> sendHeaderFields bSock hdrs

sendStatusLine :: BufferedSocket -> Version -> StatusCode -> IO ()
sendStatusLine bSock version statusCode = do
  send versionString
  send " "
  send statusCodeString
  print "Sent data"
  where
    send = BS.send bSock :: BL.ByteString -> IO ()
    versionString = Version.toString version
    statusCodeString = fromJust $ StatusCode.toString statusCode


sendHeaderFields :: BufferedSocket -> [(B.ByteString, B.ByteString)] -> IO ()
sendHeaderFields bSock hdrs = do
  dateHeader <- makeDateHeader
  forM_ (dateHeader : hdrs) (\(key,value) -> send key >> send ": " >> send value >> send crlf)
  send crlf
  where
    send = BS.send bSock 


makeDateHeader :: IO (B.ByteString, B.ByteString)
makeDateHeader =  makeHTTPDate >>= return . (\n -> ("Date" , n)) 
               

chunkedHeader :: (B.ByteString, B.ByteString)
chunkedHeader = ("Transfer-Encoding", "chunked")
