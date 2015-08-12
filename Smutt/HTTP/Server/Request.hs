{-----------------------------------------------------------------------------------------
Module name: Request
Made by:     Tomas Möre 2015

It is reccomended to import this module qualified as HTTP

------------------------------------------------------------------------------------------}

{-# LANGUAGE OverloadedStrings #-}

module Smutt.HTTP.Server.Request where


import Smutt.HTTP.Common
import Smutt.HTTP.Server.Settings
import Smutt.HTTP.Cookie

import Smutt.Utility.Utility

import qualified Smutt.HTTP.Headers as H
import qualified Smutt.Utility.URL as URL

import qualified Data.ByteString      as B
import qualified Data.ByteString.Char8 as B (words) 
import qualified Data.ByteString.Internal  as BI
import qualified Data.ByteString.Lazy  as BL
--import Network.HTTP.Types


import System.IO.Unsafe
import Data.Monoid
import Data.Maybe
import Data.Word

import Data.IORef
import Data.Either
import Data.Functor 

import Control.Monad

import qualified Network.BufferedSocket as BS  
import qualified Smutt.HTTP.BufferedSocketEXT as BS 


type RequestHeaders = Headers

type RequestBody    = BL.ByteString

type ParseingConditions = [(Bool, ParsingResult)]


-- Number or 0. If 0 Nothing more will be read from the request
type RemainingBody = IORef Int

-- If it is an "Just Int" the current chunk has more to be read. Or all is read. If it is a 0 the next read will jump to the next chunk. 
-- When all chunks have been read  it has the value "Nothing"
type RemainingFragment = IORef (Maybe Int) 


-- The body reader is made to keep track on how much has been read.
data BodyReader = StandardBody RemainingBody BufferedSocket 
                | ChunkedBody  RemainingFragment BufferedSocket  


-- a Request is the main thing in HTTP this is what the library user mainly will work with
data Request      = Request {  requestMethod       :: Method
                             , requestPathRaw      :: PathString 
                             , requestQueryRaw     :: QueryRaw
                             , requestPath         :: PathFragments
                             , requestQuery        :: Query

                             , requestHost         :: Host 
                             , requestVersion      :: Version
                             , requestHeaders      :: RequestHeaders
                             , requestCookies      :: Cookies
                             , bufSocket           :: BufferedSocket
                             , requestBody         :: RequestBody
                             , requestBodyReader    :: BodyReader 
                             } 

-- This is a debug feature, not quite meant to be used commonly. 

instance Show Request where
  show httpRequest =  let bSocket = bufSocket httpRequest
                      in  unwords [ "<HTTPRequest"
                                  ,"Type: "
                                  , show $ requestMethod httpRequest
                                  , "  Version: "
                                  , show $ requestVersion httpRequest
                                  ,  "  Path: "
                                  , show $ requestPath httpRequest
                                  , "  Headers: "
                                  , show $ requestHeaders httpRequest
                                  , ">>"]

-- functions to check the diffrent kinds of Methods of a request. 
reqIsOPTIONS = isOPTIONS  . requestMethod
reqIsPOST    = isPOST     . requestMethod
reqIsGET     = isGET      . requestMethod
reqIsHEAD    = isHEAD     . requestMethod
reqIsPUT     = isPUT      . requestMethod
reqIsDELETE  = isDELETE    . requestMethod
reqIsTRACE   = isTRACE    . requestMethod
reqIsCONNECT = isCONNECT  . requestMethod

reqIsHTTP11 = isHTTP11  . requestVersion 
reqIsHTTP10 = isHTTP10  . requestVersion
reqIsHTTP09 = isHTTP09  . requestVersion


reqHasHeader header = isJust . lookup header . requestHeaders  


data ParsingResult =  ParsingSuccess Request 
                      | InvalidVersion 
                      | InvalidMethod 
                      | HeaderLineTooLarge
                      | HeaderLimitReached 
                      | InvalidRequestLine 
                      | LengthRequired
                      | MissingHeaders
                      | MissingHost 
                      | URLTooLarge
                      | ClientQuit
                      | QueryInvalidlyPercentEncoded
    deriving (Show)

connectionClosed ClientQuit = True
connectionClosed _          = False

data HeaderResult =   HeaderSuccess [ByteString]  
                    | HasMaxLineLength 
                    | TooManyHeaders
    deriving (Eq)



protocolCanKeepAlive:: ParsingResult -> Bool
protocolCanKeepAlive (ParsingSuccess a) = (>=HTTP11) $ requestVersion a 
protocolCanKeepAlive _                  = False




requestIsHTTP11  = isHTTP11 . requestVersion
requestIsHTTP10  = isHTTP10 . requestVersion
requestIsHTTP09  = isHTTP09 . requestVersion





send100Continue:: BufferedSocket  -> Version -> Maybe (IO ())
send100Continue bSocket HTTP11 = Just $ void $ BS.send bSocket  ("HTTP/1.1 100 Continue\n\r\n\r" ::ByteString) 
send100Continue _  _ = Nothing



-- This Reads all the HTTP headers from the socket
-- Same as above procedures this requires a state to run.
readHeadersReal :: BufferedSocket -> MaxLineLength ->  Int -> Int -> IO [ByteString] -> IO HeaderResult
readHeadersReal bSocket maxLineLength maxNrHeaders iteration headers=
    case iteration >= maxNrHeaders of 
        True -> return TooManyHeaders
        False ->   do lineResult <- BS.getLine maxLineLength bSocket
                      case lineResult of 
                        Nothing   -> return HasMaxLineLength
                        Just ""   -> fmap HeaderSuccess headers
                        Just line -> readHeadersReal 
                                        bSocket 
                                        maxLineLength 
                                        maxNrHeaders 
                                        (iteration + 1) 
                                        (fmap  (line:) headers)
-- This function is simply the statring function for above
readHeaders :: BufferedSocket -> MaxLineLength -> Int -> IO HeaderResult
readHeaders bSocket maxLineLength maxNrHeaders = readHeadersReal bSocket  maxLineLength maxNrHeaders 0 (return [])


type MaxChunkSize = Int 
type RemainingSize = Int 

type ToRead = Maybe Int 

calcReadSize :: MaxChunkSize -> RemainingSize -> (ToRead, RemainingSize)
calcReadSize 0 _ = (Nothing, 0)
calcReadSize _ 0 = (Nothing, 0)
calcReadSize maxChunkSize remainingSize 
  | maxChunkSize >= remainingSize = (Just remainingSize, 0)
  | maxChunkSize <  remainingSize = (Just  maxChunkSize, remainingSize -  maxChunkSize)

readBodyChunk :: MaxChunkSize -> BodyReader -> IO (Maybe ByteString)

readBodyChunk maxChunkSize (StandardBody remainingBodyRef bSocket) = do 
  (maybeReadSize, newRemainingBody) <- calcReadSize maxChunkSize <$> readIORef remainingBodyRef
  case maybeReadSize of 
    Nothing -> return Nothing
    Just readSize -> do 
      writeIORef remainingBodyRef newRemainingBody
      Just <$> BS.readRaw bSocket readSize

readBodyChunk maxChunkSize (ChunkedBody remainingBodyRef bSocket) = do
  maybeRemainingChunk <-  readIORef remainingBodyRef
  case maybeRemainingChunk of 
    Nothing -> return Nothing 
    Just remainingChunk -> do 
      realRemainingChunk <- if remainingChunk == 0 
                              then BS.readStartOfChunk bSocket  
                              else return remainingChunk
      if realRemainingChunk == 0
        then writeIORef remainingBodyRef Nothing >> return Nothing
        else do
           let (maybeReadSize, newRemainingBody) = calcReadSize maxChunkSize realRemainingChunk
           writeIORef remainingBodyRef maybeReadSize
           case maybeReadSize of 
            Nothing -> return Nothing 
            Just readSize -> Just <$> BS.readRaw bSocket readSize


makeBodyStringList :: BodyReader -> MaxChunkSize -> IO [B.ByteString]
makeBodyStringList bodyReader maxSize = do 
  maybeChunk <- readBodyChunk maxSize bodyReader
  case maybeChunk of 
    Nothing  -> return []
    Just str -> do 
      next <- unsafeInterleaveIO $ makeBodyStringList bodyReader maxSize
      return (str : next) 
  

makeBodyString ::BodyReader -> MaxChunkSize -> IO BL.ByteString
makeBodyString bodyReader maxSize = BL.fromChunks <$>  makeBodyStringList bodyReader maxSize

type MaybeBodyLength = Maybe Int 

makeBodyReader :: MaybeBodyLength -> BufferedSocket -> IO BodyReader 
makeBodyReader Nothing bSocket = do 
  remainingRef <- newIORef (Just 0)
  return $ ChunkedBody remainingRef bSocket 

makeBodyReader (Just bodyLength) bSocket = do 
   remainingRef <- newIORef bodyLength
   return $ StandardBody remainingRef bSocket 


readToEndOfRequest :: BodyReader -> MaxChunkSize -> IO () 
readToEndOfRequest bodyReader maxChunkSize = do 
  maybeData <- readBodyChunk  maxChunkSize bodyReader
  when (isJust maybeData) (readToEndOfRequest bodyReader maxChunkSize)

-- Reads the full http request from a bufferd socket
-- Be a bit cautious as it is possible that this can fail and throw an exception
readRequest :: BufferedSocket  -> ServerSettings -> IO ParsingResult
readRequest bSocket settings =
  do 
    maybeFirstLine                  <- BS.getLine (maxPathLegnth settings) bSocket
    headerStrListAttempt            <- readHeaders bSocket (maxHeaderLength settings) (maxHeaderCount settings)

    -- Warding a few of these statments are unsafe and might throw an exception if not handeled carefully.
    -- 
    let
        Just firstLine                             = maybeFirstLine

        HeaderSuccess headerStrList                = headerStrListAttempt 
        firstLineSplit                             = B.words firstLine
        (strRequestType:strPathFull:strVersion:_)  = firstLineSplit

        versionTest                                = parseVersion strVersion
        Just version                               = versionTest

        requestMethodTest                          = parseMethod strRequestType
        Just requestMethod                         = requestMethodTest

        headerList                                 = parseHeaders headerStrList

        cookieList                                 = case lookup H.Cookie headerList of
                                                            Nothing           -> []
                                                            Just cookieString -> parseCookies cookieString

        urlAttempt                                 = URL.parse strPathFull
        Just url                                   = urlAttempt

        maybeURLHost                               = urlAttempt >>= Just . URL.getHost 

        host                                       = case (lookup H.Host headerList) of
                                                        Nothing -> maybeURLHost
                                                        a       -> a

        (pathStr, query'nFrag)                     = B.breakByte (BI.c2w '?') strPathFull
        (queryStr, fragment)                       = B.breakByte (BI.c2w '#') query'nFrag

        pathSplit                                  = splitPath $ case urlAttempt of 
                                                                        Nothing -> pathStr
                                                                        _       -> fromMaybe "" $ URL.getPath url
        query                                      = parseQuery $ case urlAttempt of 
                                                                        Nothing -> queryStr
                                                                        _       -> fromMaybe "" $ URL.getQuery url

        queryRaw                                  = fromMaybe "" $ URL.getQuery url
        pathRaw                                   = fromMaybe "" $ URL.getPath url


        isChunked                                 = and [ isHTTP11 version
                                                        , case lookup H.TransferEncoding headerList of 
                                                              Just "chunked" -> True
                                                              _              -> False
                                                          ]

        maybeContentLength                        = (lookup H.ContentLength headerList)

        maybeContentLengthInt                     =  (maybe Nothing byteStringToInteger maybeContentLength)

        bodyReader                                = let maybeBodyLength =  if isChunked 
                                                                            then Nothing 
                                                                            else case maybeContentLengthInt of 
                                                                              Nothing -> Just 0 
                                                                              a       -> a 
                                                    in unsafePerformIO $ makeBodyReader maybeBodyLength bSocket

        bodyString                                = unsafePerformIO $ makeBodyString bodyReader (tcpChunkMaxSize settings)

        requestData                                = Request {  requestMethod       = requestMethod

                                                              , requestPath         = pathSplit
                                                              , requestQuery        = fromMaybe [] query

                                                              , requestPathRaw         = pathRaw
                                                              , requestQueryRaw        = queryRaw 

                                                              , requestHost         = fromMaybe "" host
                                                              , requestVersion      = version
                                                              , requestHeaders      = headerList
                                                              , requestCookies      = cookieList   
                                                              , bufSocket           = bSocket
                                                              , requestBodyReader   = bodyReader
                                                              , requestBody         = bodyString
                                                              }

         -- These requirements will be checked first. If these are a success the server might send a "100 Continue" response once (if the httpversion is 1.1) 
        essesialRequirements1                      = [ ( isNothing maybeFirstLine    , URLTooLarge)
                                                     , ( (length firstLineSplit) < 3 , InvalidRequestLine)
                                                     , ( isNothing versionTest       ,  InvalidVersion)
                                                     , ( isNothing requestMethodTest ,  InvalidMethod)]


   

        conditionList:: ParseingConditions
        conditionList = [  ( headerStrListAttempt == HasMaxLineLength                    , HeaderLineTooLarge)
                        ,  ( headerStrListAttempt == TooManyHeaders                      , HeaderLimitReached)

                        ,  ( isNothing query                                             , QueryInvalidlyPercentEncoded) 
                           -- HTTP/1.1 requirement 
                        ,  ( and [ isHTTP11 version, isNothing host]                            , MissingHost) 
                        ,  ( and [(isPOST requestMethod), (isNothing maybeContentLengthInt) ]   , LengthRequired)
                        ,  ( and [(not $ isPOST requestMethod), (isJust maybeContentLengthInt) ] , InvalidMethod)
                        ,  ( otherwise ,  ParsingSuccess requestData)
                        ]

    case lookup True essesialRequirements1 of
        Just a  ->  return a
        Nothing ->  (fromMaybe (return ()) $ send100Continue bSocket version) >>
                    case lookup True conditionList of
                        Just a -> return a
                        Nothing -> error "Something fucked up badly"
                            





