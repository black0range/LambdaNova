{-----------------------------------------------------------------------------------------
Module name: HTTP
Made by:     Tomas Möre 2015


------------------------------------------------------------------------------------------}

{-# LANGUAGE OverloadedStrings #-}
module HTTP where

import Cookie
import Util
import qualified Headers as H
import qualified URL as URL

import qualified Data.ByteString      as B
import qualified Data.ByteString.Char8 as B (words) 
import qualified Data.ByteString.Internal  as BI
--import Network.HTTP.Types


import System.IO.Unsafe
import Data.Monoid
import Data.Maybe
import Data.Word

import Data.IORef
import Data.Maybe
import Data.Either

import Control.Monad


type MaxLineLength    = Int

type PathString       = ByteString

type PathFragment     = ByteString
type PathFragments    = [PathFragment]

type ContentLength = Integer
type StatusCode    = Int

type Query   = [(ByteString, ByteString)]
type Header  = H.Header
type Headers = H.Headers

type RequestHeaders = Headers

type Host = ByteString

type ResponseHeaders = Headers

type HTTPParseingConditions = [(Bool, HTTPParsingResult)]

data HTTPVersion = HTTP09 | HTTP10 | HTTP11
    deriving (Eq, Ord)
instance Show HTTPVersion where 
    show HTTP09 = "HTTP/0.9"
    show HTTP10 = "HTTP/1.0"
    show HTTP11 = "HTTP/1.1"

data HTTPMethod = OPTIONS | GET | HEAD | POST | PUT | DELETE | TRACE | CONNECT 
    deriving(Show, Eq)

data HTTPRequest      = HTTPRequest {    requestMethod       :: HTTPMethod
                                       , requestPath         :: PathFragments
                                       , resutQuery          :: Query
                                       , requestHost         :: Host 
                                       , requestHTTPVersion  :: HTTPVersion
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
A Full Response sends a full ByteString as a response.
The server will count the length of the ByteString and send the apporpiate content length.

If keep alive is set in server options and |Connection: Close| is not part of the header fields.
The server will keep listening on the connection

-----------------
FullLazyResponse
-----------------
Same as as full response but takes a lazy list.
This requires the |ContentLength| field to be set.
This enables space effitient sending of data


If |ContentLength| is set to |Nothing| the server will calculate the length of the entire list before sending. 
This method however might be verry performance space unefficient

WARNING! If ContentLength IS set and there are errors in the chunks there server Will NOT be able to send an error message


-----------------
ChukedResponse
-----------------
Chunked response accepts a list of Strict ByteStrings 

If there 
-}

data Response =   FullResponse      !StatusCode !ResponseHeaders !ByteString
                | FullLazyResponse  !StatusCode !ResponseHeaders [ByteString] !(Maybe ContentLength)
                | ChukedResponse    !StatusCode !ResponseHeaders [ByteString]
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
                        | URLTooLarge
    deriving (Show)

data HeaderResult =   HeaderSuccess [ByteString]  
                    | HasMaxLineLength 
                    | TooManyHeaders
    deriving (Eq)





splitPath :: PathString -> PathFragments
splitPath pathStr = filter (/=B.empty) (B.split (BI.c2w '/') pathStr)

parseQuery :: ByteString -> Query
parseQuery  =  filter (\(a,b) -> a /= "") . map (B.breakByte (BI.c2w '=')) . B.split (BI.c2w '&')


parseVersion :: ByteString -> Maybe HTTPVersion
parseVersion "HTTP/0.9" = Just HTTP09
parseVersion "HTTP/1.0" = Just HTTP10
parseVersion "HTTP/1.1" = Just HTTP11
parseVersion _          = Nothing

versionToString  :: HTTPVersion -> ByteString
versionToString HTTP09 = "HTTP/0.9"
versionToString HTTP10 = "HTTP/1.0"
versionToString HTTP11 = "HTTP/1.1"

parseMethod :: ByteString -> Maybe HTTPMethod 
parseMethod "OPTIONS"     = Just OPTIONS
parseMethod "GET"         = Just GET
parseMethod "HEAD"        = Just HEAD
parseMethod "POST"        = Just  POST
parseMethod "PUT"         = Just PUT
parseMethod "DELETE"      = Just  DELETE
parseMethod "TRACE"       = Just TRACE
parseMethod "CONNECT"     = Just  CONNECT
parseMethod _ = Nothing

-- constant for emty Header
emptyHeader :: Header
emptyHeader = (H.stringToName B.empty, B.empty)


isEmptyHeader :: Header -> Bool
isEmptyHeader a = a == emptyHeader

headerSplitter :: ByteString -> Header
headerSplitter a =  let (name, valueRaw) = B.breakByte (BI.c2w  ':') a
                        value = stripWhitespace $ B.tail valueRaw
                    in (H.stringToName name, value) 

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

send100Continue:: (ByteString -> IO Int)   -> HTTPVersion -> Maybe (IO ())
send100Continue sender HTTP11 = Just $ void $ sender "HTTP/1.1 100 Continue\n\r\n\r" 
send100Continue _  _ = Nothing

-- Reads the full http request from a bufferd socket
-- Be a bit cautious as it is possible that this can fail and throw an exception
readHTTPRequst :: BufferedSocket -> MaxLineLength -> (ByteString -> IO Int) -> IO HTTPParsingResult
readHTTPRequst bSocket maxLength send =
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

         -- These requirements will be checked first. If these are a success the server might send a "100 Continue" response once (if the httpversion is 1.1) 
        essesialRequirements1                      = [ ( isNothing maybeFirstLine    , URLTooLarge)
                                                     , ( (length firstLineSplit) < 3 , InvalidRequestLine)
                                                     , ( isNothing versionTest       ,  InvalidVersion)
                                                     , ( isNothing requestMethodTest ,  InvalidMethod)]
   

        conditionList:: HTTPParseingConditions
        conditionList = [  ( headerStrListAttempt == HasMaxLineLength                    , HeaderLineTooLarge)
                        ,  ( headerStrListAttempt == TooManyHeaders                      , HeaderLimitReached)
                           -- HTTP/1.1 requirement 
                        ,  ( and [ version == HTTP11, isNothing host]                    , MissingHost) 
                        ,  ( and [ ([] == headerList), (requestMethod == POST)]          , LengthRequired)
                        ,  ( otherwise ,  ParsingSuccess $ (HTTPRequest {  requestMethod       = requestMethod
                                                                         , requestPath         = pathSplit
                                                                         , resutQuery          = query
                                                                         , requestHost         = fromMaybe "" host
                                                                         , requestHTTPVersion  = version
                                                                         , requestHeaders      = headerList
                                                                         , requestCookies      = cookieList   
                                                                         , bufSocket           = bSocket
                                                                        }))
                        ]


    case lookup True essesialRequirements1 of
        Just a  ->  return a
        Nothing ->  (fromMaybe (return ()) $ send100Continue send version) >>
                    case lookup True conditionList of
                        Just a -> return a
                        Nothing -> error "Something fucked up badly"
                            





