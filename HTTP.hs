{-# LANGUAGE OverloadedStrings #-}
module HTTP where

import Cookie
import Util

import qualified Data.CaseInsensitive as CI
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString           as B
import Network.HTTP.Types


type MaxLineLength    = Int

type PathFragment     = ByteString
type PathFragments    = [PathFragment]

type Headers          = [Header]



data HTTPRequest      = HTTPRequest {    requestMethod       :: StdMethod
                                       , requestPath         :: PathFragments
                                       , resutQuery          :: Query
                                       , requestHTTPVersion  :: HttpVersion
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
                                  , "\nVersion: "
                                  , show $ requestHTTPVersion httpRequest
                                  ,  "\nPath: "
                                  , show $ requestPath httpRequest
                                  , "\nHeaders: "
                                  , show $ requestHeaders httpRequest
                                  , "\nbSocket: "
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



data Response =   FullResponse   Status ResponseHeaders ByteString
                | ChukedResponse Status ResponseHeaders [ByteString]
                | ManualResponse 

data HTTPParsingResult = ParsingSuccess HTTPRequest | InvalidVersion | InvalidMethod | HeaderLimitReached | InvalidRequestLine | MissingHeaders 
    deriving (Show)


parseVersion :: ByteString -> Maybe HttpVersion
parseVersion "HTTP/0.9" = Just http09
parseVersion "HTTP/1.0" = Just http10
parseVersion "HTTP/1.1" = Just http11
parseVersion _          = Nothing

-- constant for emty Header
emptyHeader :: Header
emptyHeader = (CI.mk B.empty, B.empty)


isEmptyHeader :: Header -> Bool
isEmptyHeader a = a == emptyHeader

headerSplitter :: ByteString -> Header
headerSplitter a =  let (name, valueRaw) = B.breakByte (BI.c2w  ':') a
                        value = stripWhitespace $ B.tail valueRaw
                    in (CI.mk name, value) 

parseHeaders :: [ByteString] -> RequestHeaders
parseHeaders inData  = [splitted | raw <- inData, let splitted = headerSplitter raw,  not $ isEmptyHeader splitted ] 




-- This function gets a line from the BufferedSocket.
-- However this is required to accept lines that are a maximum length of the buffer size
-- Probably should change this in the future somehow
getLineHTTPReal :: BufferedSocket -> ByteString -> Int -> MaxLineLength -> IO ByteString
getLineHTTPReal bSocket buffered length maxLength = 
  if length >= maxLength
    then
        error "HTTP max line length reached"
    else 
        do
            (stringLength, string) <-  bufferedSocketRead bSocket
            let crlfSplit = B.breakSubstring crlf string
            case crlfSplit of
                        (_,"")               -> getLineHTTPReal bSocket (B.append buffered string) (length + stringLength) maxLength
                        (returnString, rest) -> do   
                                                    writeIORef bufferDataRef    (B.drop crlfLength rest)
                                                    writeIORef bytesInBufferRef (stringLength - ((B.length returnString) + crlfLength)) 
                                                    return returnString
                      
    where 
       (_,_,_, bufferDataRef, bytesInBufferRef) = bSocket
-- Starts the "Real" function.
-- This is also responsible for rebuffering the buffer if necssesarry
getLineHTTP ::  MaxLineLength -> BufferedSocket -> IO ByteString  
getLineHTTP maxLength bSocket  =
    do  
        bufferData    <- readIORef bufferDatRef
        bytesInBuffer <- readIORef bytesInBufferRef
        let crlfSplit = case bytesInBuffer of 
                                0         -> ("","")
                                otherwise -> B.breakSubstring crlf bufferData
        case crlfSplit of
                    (_,"")               -> getLineHTTPReal bSocket bufferData bytesInBuffer maxLength
                    (returnString, rest) -> do 
                                             writeIORef bufferDatRef     (B.drop crlfLength rest)
                                             writeIORef bytesInBufferRef (bytesInBuffer - ((B.length returnString) + crlfLength))
                                             return returnString
                    

  where 
    (_,_,_, bufferDatRef, bytesInBufferRef) = bSocket

-- This Reads all the HTTP headers from the socket
-- Same as above procedures this requires a state to run.
readHTTPHeadersReal :: BufferedSocket -> MaxLineLength ->  Int -> Int -> IO [ByteString] -> IO (Maybe [ByteString])
readHTTPHeadersReal bSocket maxLineLength maxNrHeaders iteration headers=
    case iteration >= maxNrHeaders of 
        True -> return Nothing
        False ->   do
                      line <- getLineHTTP maxLineLength bSocket
                      if line == ""
                        then 
                           fmap Just headers
                        else 
                          readHTTPHeadersReal bSocket maxLineLength maxNrHeaders (iteration + 1) (fmap  (line:) headers)
        
-- This function is simply the statring function for above
readHTTPHeaders :: BufferedSocket -> MaxLineLength -> Int -> IO (Maybe [ByteString])
readHTTPHeaders bSocket maxLineLength maxNrHeaders = readHTTPHeadersReal bSocket  maxLineLength maxNrHeaders 0 (return [])

-- Reads the full http request from a bufferd socket
-- Be a bit cautious as it is possible that this can fail and throw an exception
readHTTPRequst :: BufferedSocket -> MaxLineLength -> IO HTTPParsingResult
readHTTPRequst bSocket maxLength =
  do 
    firstLine            <- getLineHTTP maxLength bSocket
    headerStrListAttempt <- readHTTPHeaders bSocket maxLength 100
    let   
          Just headerStrList                         = headerStrListAttempt

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
                                                            
          (pathStr, query'nFrag)                     =  case B.elemIndex (BI.c2w '?') strPathFull of 
                                                            Just index ->  let path  = B.take index strPathFull
                                                                               query = B.drop index strPathFull
                                                                            in (path, query)
                                                            Nothing    ->   (strPathFull, B.empty)
          (queryStr, fragment)                       = B.breakByte (BI.c2w '#') query'nFrag

          pathSplit                                  = filter (/=B.empty) (B.split (BI.c2w '/') pathStr)
          query                                      = parseQuery queryStr
          conditionList::[(Bool, HTTPParsingResult)]
          conditionList = [   (  (length firstLineSplit) < 3   ,  InvalidRequestLine)
                            , (  isNothing headerStrListAttempt ,  HeaderLimitReached)
                            , (  isNothing versionTest          ,  InvalidVersion)
                            , (  isLeft    requestMethodTest    ,  InvalidMethod)
                            , (  and [([] == headerList), (requestMethod == POST)] ,  MissingHeaders)
                            , (  otherwise ,  ParsingSuccess (HTTPRequest {   requestMethod       = requestMethod
                                                                            , requestPath         = pathSplit
                                                                            , resutQuery          = query
                                                                            , requestHTTPVersion  = version
                                                                            , requestHeaders      = headerList
                                                                            , requestCookies      = cookieList   
                                                                            , bufSocket           = bSocket
                                                                           }))
                            ]
    case lookup True conditionList of
        Just a  -> return a
        Nothing -> error "Someone just fucked up badly"  




