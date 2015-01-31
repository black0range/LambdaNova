{-----------------------------------------------------------------------------------------
Module name: HTTP
Made by:     Tomas Möre 2015


------------------------------------------------------------------------------------------}

{-# LANGUAGE OverloadedStrings #-}
module HTTP where

import Cookie
import Util
import ServerOptions

import qualified Headers as H
import qualified URL as URL

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
import Data.Maybe
import Data.Either

import Control.Monad

import qualified BufferedSocket as BS  

type BufferedSocket = BS.BufferedSocket
type MaxLineLength  = BS.MaxLineLength

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

type ParseingConditions = [(Bool, ParsingResult)]
type RequestBody            = IO BL.ByteString

data Version = HTTP09 | HTTP10 | HTTP11
    deriving (Eq, Ord)

instance Show Version where 
    show HTTP09 = "HTTP/0.9"
    show HTTP10 = "HTTP/1.0"
    show HTTP11 = "HTTP/1.1"

data Method = OPTIONS | GET | HEAD | POST | PUT | DELETE | TRACE | CONNECT 
    deriving(Show, Eq)

isOPTIONS = (==OPTIONS)
isPOST    = (==POST)
isGET     = (==GET)
isHEAD    = (==HEAD)
isPUT     = (==PUT)
isDELETE  = (==DELETE)
isTRACE   = (==TRACE)
isCONNECT = (==CONNECT)

data Request      = Request {  requestMethod       :: Method
                             , requestPath         :: PathFragments
                             , requestQuery        :: Query
                             , requestHost         :: Host 
                             , requestVersion      :: Version
                             , requestHeaders      :: RequestHeaders
                             , requestCookies      :: Cookies
                             , bufSocket           :: BufferedSocket
                             , requestBody         :: RequestBody
                             } 


instance Show Request where
  show httpRequest =  let (sock, buffer, bufSize, bufferDataRef) = bufSocket httpRequest
                          buffered     = unsafePerformIO (readIORef bufferDataRef)
                      in  unwords [ "<HTTPRequest"
                                  ,"Type: "
                                  , show $ requestMethod httpRequest
                                  , "  Version: "
                                  , show $ requestVersion httpRequest
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
                | FullLazyResponse  !StatusCode !ResponseHeaders BL.ByteString !(Maybe ContentLength)
                | ChukedResponse    !StatusCode !ResponseHeaders BL.ByteString
                | ManualResponse 

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

splitPath :: PathString -> PathFragments
splitPath pathStr = filter (/=B.empty) (B.split (BI.c2w '/') pathStr)

parseQuery :: ByteString -> Query
parseQuery  =  filter (\(a,b) -> a /= "") . map (B.breakByte (BI.c2w '=')) . B.split (BI.c2w '&')


parseVersion :: ByteString -> Maybe Version
parseVersion "HTTP/0.9" = Just HTTP09
parseVersion "HTTP/1.0" = Just HTTP10
parseVersion "HTTP/1.1" = Just HTTP11
parseVersion _          = Nothing

isHTTP11 = (==HTTP11) 
isHTTP10 = (==HTTP10)
isHTTP09 = (==HTTP09)

requestIsHTTP11 = isHTTP11 . requestVersion
requestIsHTTP10 = isHTTP10 . requestVersion
requestIsHTT09  = isHTTP11 . requestVersion

versionToString  :: Version -> ByteString
versionToString HTTP09 = "HTTP/0.9"
versionToString HTTP10 = "HTTP/1.0"
versionToString HTTP11 = "HTTP/1.1"

parseMethod :: ByteString -> Maybe Method 
parseMethod "OPTIONS"     = Just OPTIONS
parseMethod "GET"         = Just GET
parseMethod "HEAD"        = Just HEAD
parseMethod "POST"        = Just POST
parseMethod "PUT"         = Just PUT
parseMethod "DELETE"      = Just DELETE
parseMethod "TRACE"       = Just TRACE
parseMethod "CONNECT"     = Just CONNECT
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




send100Continue:: (ByteString -> IO Int)   -> Version -> Maybe (IO ())
send100Continue sender HTTP11 = Just $ void $ sender "HTTP/1.1 100 Continue\n\r\n\r" 
send100Continue _  _ = Nothing


-- This Reads all the HTTP headers from the socket
-- Same as above procedures this requires a state to run.
readHeadersReal :: BufferedSocket -> MaxLineLength ->  Int -> Int -> IO [ByteString] -> IO HeaderResult
readHeadersReal bSocket maxLineLength maxNrHeaders iteration headers=
    case iteration >= maxNrHeaders of 
        True -> return TooManyHeaders
        False ->   do
                      lineResult <- BS.getLine maxLineLength bSocket
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






-- Reads the full http request from a bufferd socket
-- Be a bit cautious as it is possible that this can fail and throw an exception
readRequest :: BufferedSocket -> (ByteString -> IO Int) -> ServerSettings -> IO ParsingResult
readRequest bSocket  send settings =
  do 
    maybeFirstLine          <- BS.getLine (maxPathLegnth settings) bSocket
    headerStrListAttempt    <- readHeaders bSocket (maxHeaderLength settings) (maxHeaderCount settings)

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

        

        isChunked                                 = and [ isHTTP11 version
                                                        , case lookup H.TransferEncoding headerList of 
                                                              Just "chunked" -> True
                                                              _              -> False
                                                          ]

        maybeContentLength                        = (lookup H.ContentLength headerList)

        maybeContentLengthInt                     =  (maybe Nothing byteStringToInteger maybeContentLength)

        bodyReader                                = do
                                                        if isPOST requestMethod
                                                            then
                                                                if isChunked 
                                                                    then 
                                                                        BS.readChunked bSocket
                                                                    else 
                                                                        do 
                                                                            let Just len = maybeContentLengthInt 
                                                                            bodyList <- unsafeInterleaveIO $ BS.readNLazy bSocket len
                                                                            return $ bodyList 
                                                            else 
                                                                return []

        requestData                                = Request {  requestMethod       = requestMethod
                                                              , requestPath         = pathSplit
                                                              , requestQuery        = query
                                                              , requestHost         = fromMaybe "" host
                                                              , requestVersion      = version
                                                              , requestHeaders      = headerList
                                                              , requestCookies      = cookieList   
                                                              , bufSocket           = bSocket
                                                              , requestBody         = fmap BL.fromChunks bodyReader
                                                                  }

         -- These requirements will be checked first. If these are a success the server might send a "100 Continue" response once (if the httpversion is 1.1) 
        essesialRequirements1                      = [ ( isNothing maybeFirstLine    , URLTooLarge)
                                                     , ( (length firstLineSplit) < 3 , InvalidRequestLine)
                                                     , ( isNothing versionTest       ,  InvalidVersion)
                                                     , ( isNothing requestMethodTest ,  InvalidMethod)]


   

        conditionList:: ParseingConditions
        conditionList = [  ( headerStrListAttempt == HasMaxLineLength                    , HeaderLineTooLarge)
                        ,  ( headerStrListAttempt == TooManyHeaders                      , HeaderLimitReached)
                           -- HTTP/1.1 requirement 
                        ,  ( and [ isHTTP11 version, isNothing host]                                    , MissingHost) 
                        ,  ( and [(isPOST requestMethod), (isNothing maybeContentLengthInt) ]          , LengthRequired)
                        ,  ( otherwise ,  ParsingSuccess requestData)
                        ]


    case lookup True essesialRequirements1 of
        Just a  ->  return a
        Nothing ->  (fromMaybe (return ()) $ send100Continue send version) >>
                    case lookup True conditionList of
                        Just a -> return a
                        Nothing -> error "Something fucked up badly"
                            





