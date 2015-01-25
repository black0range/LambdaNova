{-----------------------------------------------------------------------------------------
Module name: HTTP
Made by:     Tomas Möre 2015


------------------------------------------------------------------------------------------}

{-# LANGUAGE OverloadedStrings #-}
module HTTP where

import Cookie
import Util
import qualified Headers as H
import qualified URI as URI
import qualified Data.ByteString      as B
import qualified Data.ByteString.Char8 as B (words) 
import qualified Data.ByteString.Internal  as BI
--import Network.HTTP.Types


import System.IO.Unsafe

import Data.Word

import Data.IORef
import Data.Maybe
import Data.Either


type MaxLineLength    = Int

type PathString       = ByteString

type PathFragment     = ByteString
type PathFragments    = [PathFragment]

type ContentLength = Integer
type StatusCode    = Int

type Host = Bytestring

type ResponseHeaders = [(ByteString, ByteString)]

type HTTPParseingConditions = [(Bool, HTTPParsingResult)]

data HTTPVersion = HTTP09 | HTTP10 | HTTP11

data HTTPMethod = OPTIONS | GET | HEAD | POST | PUT | DELETE | TRACE | CONNECT

data HTTPRequest      = HTTPRequest {    requestMethod       :: StdMethod
                                       , requestPath         :: PathFragments
                                       , resutQuery          :: Query
                                       , requestHost         :: Host 
                                       , requestHTTPVersion  :: HTTPVerson
                                       , requestHeaders      :: RequestHeaders
                                       , requestCookies      :: Cookies
                                       , bufSocket           :: BufferedSocket
                                       } 

instance Show HTTPRequest where
  show httpRequest =  let (sock, buffer, bufSize, bufferDataRef, bufferDataLength) = bufSocket httpRequest
                          buffered     = unsafePerformIO (readIORef bufferDataRef)
                          bufferLength = unsafePerformIO (readIORef bufferDataLength)
                      in  unwords [ "<HTTPRequest"
                                  ,"Type: "
                                  , show $ requestMethod httpRequest
                                  , "  Version: "
                                  , show $ requestHTTPVersion httpRequest
                                  ,  "  Path: "
                                  , show $ requestPath httpRequest
                                  , "  Headers: "
                                  , show $ requestHeaders httpRequest
                                  , "  bSocket: "
                                  , "<bufferedSocket "
                                  , "socket: "
                                  , show sock
                                  , " bufferAddr: "
                                  , show buffer
                                  , " bufferSize: "
                                  , show bufSize
                                  , " buffered: "
                                  , show buffered
                                  , " buffered: "
                                  , show bufferLength
                                  , ">>"]


{-
-------------
FullResponse
-------------
A Full Response sends a full Bytestring as a response.
The server will count the length of the bytestring and send the apporpiate content length.

If keep alive is set in server options and |Connection: Close| is not part of the header fields.
The server will keep listening on the connection

-----------------
FullLazyResponse
-----------------
Same as as full response but takes a lazy list.
This requires the |ContentLength| field to be set.
This enables space effitient sending of data

If |ContentLength| is set to |Nothing| the server will calculate the length of the entire list before sending.

-----------------
ChukedResponse
-----------------
Chunked response accepts a list of Bytestrings 

-}

data Response =   FullResponse      StatusCode ResponseHeaders ByteString
                | FullLazyResponse  StatusCode ResponseHeaders [ByteString] (Maybe ContentLength)
                | ChukedResponse    StatusCode ResponseHeaders [ByteString]
                | ManualResponse 

data HTTPParsingResult =  ParsingSuccess HTTPRequest 
                        | InvalidVersion 
                        | InvalidMethod 
                        | HeaderLineTooLarge
                        | HeaderLimitReached 
                        | InvalidRequestLine 
                        | LengthRequired
                        | MissingHeaders
                        | MissingHost 
                        | UriTooLarge
    deriving (Show)

data HeaderResult =   HeaderSuccess [ByteString]  
                    | HasMaxLineLength 
                    | TooManyHeaders
    deriving (Eq)





splitPath :: PathString -> PathFragments
splitPath pathStr = filter (/=B.empty) (B.split (BI.c2w '/') pathStr)



parseVersion :: ByteString -> Maybe HttpVersion
parseVersion "HTTP/0.9" = Just HTTP09
parseVersion "HTTP/1.0" = Just HTTP10
parseVersion "HTTP/1.1" = Just HTTP11
parseVersion _          = Nothing

versionToString  :: HttpVersion -> ByteString
versionToString HTTP09 = "HTTP/0.9"
versionToString HTTP10 = "HTTP/1.0"
versionToString HTTP11 = "HTTP/1.1"

-- constant for emty Header
emptyHeader :: Header
emptyHeader = (H.stringToHeaderName B.empty, B.empty)


isEmptyHeader :: Header -> Bool
isEmptyHeader a = a == emptyHeader

headerSplitter :: ByteString -> Header
headerSplitter a =  let (name, valueRaw) = B.breakByte (BI.c2w  ':') a
                        value = stripWhitespace $ B.tail valueRaw
                    in (H.stringToHeaderName name, value) 

parseHeaders :: [ByteString] -> RequestHeaders
parseHeaders inData  = [splitted | raw <- inData, let splitted = headerSplitter raw,  not $ isEmptyHeader splitted ] 




-- # Following are two very similary looking functions. The first one allwasy cheacks if the pre read data firs then if that fails it attempts to read more # --

-- This function gets a line from the BufferedSocket.
-- However this is required to accept lines that are a maximum length of the buffer size
-- Probably should change this in the future somehow
getLineHTTPReal :: BufferedSocket -> ByteString -> Int -> MaxLineLength -> IO (Maybe ByteString)
getLineHTTPReal bSocket buffered length maxLength = 
  if length >= maxLength
    then
      do 
        writeIORef bufferDataRef    buffered
        writeIORef bytesInBufferRef length 

        return Nothing 
    else 
        do
            (stringLength, string) <-  bufferedSocketRead bSocket
            let crlfSplit = B.breakSubstring crlf string
            case crlfSplit of
                        (_,"")               -> getLineHTTPReal bSocket (B.append buffered string) (length + stringLength) maxLength
                        (returnString, rest) -> do   
                                                    writeIORef bufferDataRef    (B.drop crlfLength rest)
                                                    writeIORef bytesInBufferRef (stringLength - ((B.length returnString) + crlfLength)) 
                                                    return $ Just returnString
                      
    where 
       (_,_,_, bufferDataRef, bytesInBufferRef) = bSocket

-- Starts the "Real" function.
-- This is also responsible for rebuffering the buffer if necssesarry
getLineHTTP ::  MaxLineLength -> BufferedSocket -> IO (Maybe ByteString)  
getLineHTTP maxLength bSocket  =
    do  
        bufferData    <- readIORef bufferDatRef
        bytesInBuffer <- readIORef bytesInBufferRef
        let crlfSplit = case bytesInBuffer of 
                                0         -> ("","")
                                otherwise -> B.breakSubstring crlf bufferData
        case crlfSplit of
                    (_,"")               -> getLineHTTPReal bSocket bufferData bytesInBuffer maxLength
                    (returnString, rest) -> if (B.length returnString) >= maxLength 
                                              then  
                                                return Nothing
                                              else
                                                do 
                                                 writeIORef bufferDatRef     (B.drop crlfLength rest)
                                                 writeIORef bytesInBufferRef (bytesInBuffer - ((B.length returnString) + crlfLength))
                                                 return $ Just returnString
                        

  where 
    (_,_,_, bufferDatRef, bytesInBufferRef)  = bSocket



-- This Reads all the HTTP headers from the socket
-- Same as above procedures this requires a state to run.
readHTTPHeadersReal :: BufferedSocket -> MaxLineLength ->  Int -> Int -> IO [ByteString] -> IO HeaderResult
readHTTPHeadersReal bSocket maxLineLength maxNrHeaders iteration headers=
    case iteration >= maxNrHeaders of 
        True -> return TooManyHeaders
        False ->   do
                      lineResult <- getLineHTTP maxLineLength bSocket
                      case lineResult of 
                        Nothing   -> return HasMaxLineLength
                        Just ""   -> fmap HeaderSuccess headers
                        Just line -> readHTTPHeadersReal 
                                        bSocket 
                                        maxLineLength 
                                        maxNrHeaders 
                                        (iteration + 1) 
                                        (fmap  (line:) headers)
-- This function is simply the statring function for above
readHTTPHeaders :: BufferedSocket -> MaxLineLength -> Int -> IO HeaderResult
readHTTPHeaders bSocket maxLineLength maxNrHeaders = readHTTPHeadersReal bSocket  maxLineLength maxNrHeaders 0 (return [])

send100Continue:: BufferedSocket   -> HTTPVersion -> Maybe IO ()
send100Continue (socket,_,_,_,_,_) HTTP11 = 
send100Continue _  _ = Nothing

-- Reads the full http request from a bufferd socket
-- Be a bit cautious as it is possible that this can fail and throw an exception
readHTTPRequst :: BufferedSocket -> MaxLineLength -> IO HTTPParsingResult
readHTTPRequst bSocket maxLength =
  do 
    maybeFirstLine          <- getLineHTTP maxLength bSocket
    headerStrListAttempt    <- readHTTPHeaders bSocket maxLength 100

    -- Warding a few of these statments are unsafe and might throw an exception if not handeled carefully.
    -- 
    let   
          HeaderSuccess headerStrList                = headerStrListAttempt     

          Just firstLine                             = maybeFirstLine
          firstLineSplit                             = B.words firstLine
          (strRequestType:strPathFull:strVersion:_)  = firstLineSplit

          versionTest                                = parseVersion strVersion
          Just version                               = versionTest

          requestMethodTest                          = parseMethod strRequestType
          Right requestMethod                        = requestMethodTest

          headerList                                 = parseHeaders headerStrList

          cookieList                                 = case lookup "Cookie" headerList of
                                                            Nothing           -> []
                                                            Just cookieString -> parseCookies cookieString
          host                                       = lookup H.Host headerList

          urlAttempt                                 = URI.parse strPathFull


          (pathStr, query'nFrag)                     = B.breakByte (BI.c2w '?') strPathFull
          (queryStr, fragment)                       = B.breakByte (BI.c2w '#') query'nFrag

          pathSplit                                  = splitPath $ case urlAttempt of 
                                                                        Nothing -> pathStr
                                                                        _       -> URL.getPath
          query                                      = parseQuery $ case urlAttempt of 
                                                                        Nothing -> queryStr
                                                                        _       -> URL.getQuery 

         -- These requirements will be checked first. If these are a success the server might send a "100 Continue" response once (if the httpversion is 1.1) 
          essesialRequirements1                      = [  ( isNothing maybeFirstLine    , UriTooLarge)
                                                        , ( length firstLineSplit) < 3  , InvalidRequestLine)
                                                        , ( isNothing versionTest       ,  InvalidVersion)
                                                        , ( isLeft    requestMethodTest ,  InvalidMethod)

           essesialRequirements2                     =  [ ( headerStrListAttempt == HasMaxLineLength ,  HeaderLineTooLarge)
                                                        , ( headerStrListAttempt == TooManyHeaders   ,  HeaderLimitReached)]

          http11Requirements                         = [ ( isNothign host , MissingHost)]

          conditionList::HTTPParseingConditions
          conditionList = [  ( and [ ([] == headerList)
                                     , (requestMethod == POST)]          , LengthRequired)
                            , (  otherwise ,  ParsingSuccess (HTTPRequest {   requestMethod       = requestMethod
                                                                            , requestPath         = pathSplit
                                                                            , resutQuery          = query
                                                                            , requestHTTPVersion  = version
                                                                            , requestHeaders      = headerList
                                                                            , requestCookies      = cookieList   
                                                                            , bufSocket           = bSocket
                                                                           }))
                            ]

        send100Continue                              = 


    case lookup True essesialRequirements1 of
        Just a  -> return a
        Nothing -> send100Continue
                            





