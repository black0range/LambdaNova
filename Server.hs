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


import Data.Word

import Data.IORef
import Data.Maybe
import Data.Either




import Util
import HTTP
import Cookie



type WebThunk = (HTTPRequest -> IO ())







mainHTTPVersion:: ByteString
mainHTTPVersion = "HTTP/1.1"

testResponse:: ByteString
testResponse = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"

invalidReuqestResponse :: ByteString
invalidReuqestResponse = B.append "HTTP/1.1 400 Bad Request" crlf







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
        timeStart    <- getCurrentTime
        request      <- readHTTPRequst bSocket bufferSize
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