{-----------------------------------------------------------------------------------------
Module name: LambdaNova - a web server
Made by:     Tomas Möre 2014


Usage: Pass a function of type .... to the serve fucntion exported by module




Notes for editor: Many functions are splitted into two parts, Any function with the postfix "Real"
                  has a initating function with the same name but without the postfix

------------------------------------------------------------------------------------------}
-- packCStringLen :: CStringLen -> IO ByteString
{-# LANGUAGE OverloadedStrings #-}
module HTTPServer  
( serve
, Response
) where  


import Data.Time.Clock

import Control.Concurrent
import Control.Monad
import System.IO
import System.IO.Unsafe

import Network.Socket
import Network.HTTP.Types
import qualified Network.Socket.ByteString as B
import qualified Data.ByteString           as B  hiding (pack)
import qualified Data.ByteString.Char8     as B  hiding (findSubstring, elemIndex, split, break, spanEnd, dropWhile)
import qualified Data.ByteString.Internal  as BI
import qualified Data.ByteString.Builder   as BB
import Data.IORef
import Data.Either
import Data.Maybe
import qualified Data.String as S
import Data.List
import Data.List.Split
import Data.Word
import qualified Data.CaseInsensitive as CI

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc

import Foreign.Storable



type ByteString       = B.ByteString

type MaxLineLength    = Int

type BufferSize       = Int
type BufferDataLength = IORef Int
type BufferData       = IORef ByteString

-- This buffered socket is read by first checking the bufferData ByteString. If that is not enought buffer a bit more data... etc
type BufferedSocket   = (Socket, (ForeignPtr Word8), BufferSize, BufferData, BufferDataLength)

type PathFragment     = ByteString
type PathFragments    = [PathFragment]

type Cookie           = (ByteString, ByteString)
type Cookies          = [Cookie]

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

type WebThunk = (HTTPRequest -> IO ())

data HTTPParsingResult = ParsingSuccess HTTPRequest | InvalidVersion | InvalidMethod | HeaderLimitReached | InvalidRequestLine | MissingHeaders 
    deriving (Show)

-- Constants for the newlineCharacters in CCar format
crlf :: ByteString
crlf = "\r\n"

crlfLength = 2

mainHTTPVersion:: ByteString
mainHTTPVersion = "HTTP/1.1"

testResponse:: ByteString
testResponse = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"

invalidReuqestResponse :: ByteString
invalidReuqestResponse = B.append "HTTP/1.1 400 Bad Request" crlf

-- Removes any unecessary whitespaces from the start and end of a string
stripWhitespace :: ByteString -> ByteString
stripWhitespace a = let firstClean      = B.dropWhile BI.isSpaceWord8 a
                        (finalClean, _) = B.spanEnd BI.isSpaceWord8 firstClean
                    in finalClean



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


cookieInnerSplit :: ByteString -> Cookie
cookieInnerSplit a = let (nameRaw, valueRaw) =  B.breakByte (BI.c2w '=') a
                         name                =  stripWhitespace nameRaw
                         value               =  stripWhitespace valueRaw
                     in (name, value)

parseCookies :: ByteString -> Cookies
parseCookies cookieString = 
    let cookieListRaw   = B.split (BI.c2w ';') cookieString
    in map cookieInnerSplit cookieListRaw


{- 
This puts together a new "buffered Socket". or bSock
A buffered socket is a socket which is acommodated by a Ptr. Because of the need to split headers etc up at certain points but still
safet he rest of the data to be whatever the server requires it to be.

A buffered socket should be called with freeBSocket before disregarded. 
The server thunk function does so automaticly.

-}
makeBufferedSocket :: Socket -> BufferSize -> IO BufferedSocket
makeBufferedSocket sock  bufferSize = 
                    do
                      bufferDataLength <- newIORef  0 
                      bufferData       <- newIORef  B.empty
                      buffer           <- BI.mallocByteString bufferSize
                      return (sock, buffer, bufferSize, bufferData, bufferDataLength)


bufferedSocketRead :: BufferedSocket -> IO (Int, ByteString)
bufferedSocketRead (socket, buffer, bufferSize,_,_) = 
    do 
        (read, _) <- withForeignPtr buffer socketReader
        let string = BI.fromForeignPtr buffer 0 read
        return (read, string) 
    where 
        socketReader ptr = recvBufFrom socket ptr bufferSize


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

    

-- The main thunk of the server. This code is resposible for reading the request handing it over to the "real thunk"
-- then sending the response in an appropiate manner along with closing down the socket and freeing the pointer
serverThunk :: (Socket, SockAddr) -> WebThunk -> IO ()
serverThunk (sock, sockAddr) thunk = 
    do 
        let bufferSize = 1024
            send :: ByteString -> IO Int
            send outData = B.sendTo sock outData sockAddr
        bSocket    <- makeBufferedSocket sock  bufferSize
        putStrLn $ (show sock) ++ " " ++ (show sockAddr)
        
        --(headerList, body) <- readToEndOfHeader sock
        --putStrLn  $ show headerList
        timeStart <-getCurrentTime
        request      <- readHTTPRequst bSocket 1024
        writtenBytes <- (case request of
                            ParsingSuccess  request -> send testResponse
                            _                       -> send invalidReuqestResponse)

        putStrLn (show writtenBytes)

        timeEnd <-getCurrentTime
        putStrLn  (show (diffUTCTime timeEnd timeStart))

        putStrLn (show request)  
        sClose sock


-- The server function, Currently give it a function that will process the request and it will run a new thread from every request
serve :: WebThunk -> IO ()
serve thunk = 
   withSocketsDo $ do 
    -- create socket
    sock <- socket AF_INET Stream 0
    putStrLn $ show sock
    -- make socket immediately reusable - eases debugging.
    setSocketOption sock ReuseAddr 0

    --setSocketOption sock NoDelay 1
    
    
    -- listen on TCP port 8000
    bindSocket sock (SockAddrInet 8000 iNADDR_ANY)

    -- Tells the socket that it can have max 1000 connections
    listen sock 1000

    forever $ do
                socketData <- accept sock
                newThread <- forkIO (serverThunk socketData thunk)
                return ()