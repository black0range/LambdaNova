{-----------------------------------------------------------------------------------------
Module name: LambdaNova - a web server
Made by:     Tomas Möre 2014


Usage: 	Pass a function of type (HTTPRequest -> IO Response) to the |serve| fucntion exported by module
	   	yor function is now responsible for putting together the response.
	   	An empty response will be treated as a |200 OK| without any content 
	
		This library uses STRICT strings internally. 



Notes for editor: Many functions are splitted into two parts, Any function with the postfix "Real"
                  has a initating function with the same name but without the postfix

------------------------------------------------------------------------------------------}

{-# LANGUAGE OverloadedStrings #-}
module Server  
( serve
) where  


import Data.Time.Clock

import Control.Concurrent
import Control.Monad

import System.IO
import System.IO.Unsafe

import Network.Socket

import qualified Network.Socket.ByteString as B
import qualified Data.ByteString           as B  hiding (pack)
import qualified Data.ByteString.Char8     as B  hiding (findSubstring, elemIndex, split, break, spanEnd, dropWhile)
import qualified Data.ByteString.Internal  as BI
import qualified Data.ByteString.Builder   as BB
import qualified Data.ByteString.Lazy      as BL

import Data.Word

import Data.IORef
import Data.Maybe
import Data.Either
import Data.Monoid



import Util
import HTTP
import StatusCodes


type WebThunk = (HTTPRequest -> IO Response)

type KeepGoing = Boolean




mainHTTPVersion:: ByteString
mainHTTPVersion = "HTTP/1.1"

testResponse:: ByteString
testResponse = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"

invalidReuqestResponse :: ByteString
invalidReuqestResponse = B.append "HTTP/1.1 400 Bad Request" crlf



-- This function handles the response to the request. Return a Boolean. If True 

responseHandler :: (ByteString -> IO Int) -> HTTPParsingResult -> WebThunk -> IO KeepGoing
responseHandler send (ParsingSuccess request) thunk = 
  do 
    response <- thunk request
    let (status, responseHeaders) = case  response of 
                                FullResponse   a b  _ -> (a, b)
                                ChukedResponse a b  _ -> (a, b)
          statusLine = B.concat [mainHTTPVersion, " ", statusCodeToStr status, crlf]

         headerSender :: [ByteString] -> IO ()
         headerSender a = case a of 
         					[] -> send crlf >> return () 
         					_  -> let chunk = take 10
         							  rest  = drop 10
         							  readString = B.concat chunk
         						  in (send readString) >> headerSender rest

    case response of 
    	-- If response is set to Manual we expect the user to have taken care of everything
    	ManualResponse -> return True
    	-- FullResposne asks the server to count the content and send it all. 
    	-- Full response takes a full bytestring however we still send the data in smaller sizes of max 4kB each (4 KB will be set to a setting)
      	FullResponse   _ _ fullString    -> do
                                            let contentLength = (H.ContentLength, B.length fullString)
                                            	headers       = (contentLength:responseHeaders) 
                                            	headerList    = map H.toSendRow headers

                                            	bodySender :: ByteString -> IO ()
                                            	bodySender a = let 	chunkSize = 1024 * 4) 
                                            						chunk = B.take chunkSize a
                                            						rest  = B.drop chunkSize a
                                            					in send chunk >> bodySender rest
                                            send 		 statusLine
                                            headerSender headerList
                                            bodySender   fullString
                                            send crlf
                                            let keepAlive = lookup responseHeaders H.Close
                                            case keepAlive of
                                            	Nothing 	 -> return False 
                                            	Just "Close" -> return True
        -- Full LazyResponbse is the same ass FullResponse Except that we expect a list of ByteStrings
        -- A Full Lazy Response requires a content Length parameter as well
        FullLazyResponse _ _ chunkedString contentLength ->
      	ChukedResponse _ _ chunkedString ->
      FullResponse FullResponse   Status ResponseHeaders ByteString
                | ChukedResponse Status ResponseHeaders [ByteString]
responseHandler s error _ =
  let status = case error of 
                  UriTooLarge        -> statusCodeToStr 414 --" 414 Request-URI Too Long\n\r"
                  InvalidVersion     -> statusCodeToStr 505 --" 505 HTTP Version Not Supported\n\r"
                  InvalidMethod      -> statusCodeToStr 405 --" 405 Method Not Allowed\n\r"        
                  HeaderLimitReached -> statusCodeToStr 413 --" 413 Request Entity Too Large\n\r"
                  InvalidRequestLine -> statusCodeToStr 400 --" 400 Bad Request\n\r"
                  LengthRequired     -> statusCodeToStr 411 --" 411 Length Required\n\r"
                  _                  -> statusCodeToStr 400 --" 400 Bad Request\n\r"
      response = B.concat [mainHTTPVersion, " ", status, crlf]
  in do 
      s response
      return False

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
        --timeStart    <- getCurrentTime
        request      <- readHTTPRequst bSocket bufferSize
        writtenBytes <- responseHandler send request thunk
        
        --putStrLn (show writtenBytes)
        --timeEnd <-getCurrentTime
        --putStrLn  (show (diffUTCTime timeEnd timeStart))

        --putStrLn (show request)  
        sClose sock


-- The server function, Currently give it a function that will process the request and it will run a new thread from every request
serve :: WebThunk -> IO ()
serve thunk = 
   withSocketsDo $ do 
    -- create socket
    sock <- socket AF_INET Stream 0
    putStrLn $ show sock
    -- make socket immediately reusable - eases debugging.
    setSocketOption sock ReuseAddr 1

    --setSocketOption sock NoDelay 1
    
    
    -- listen on TCP port 8000
    bindSocket sock (SockAddrInet 8000 iNADDR_ANY)

    -- Tells the socket that it can have max 1000 connections
    listen sock 1000

    forever $ do
                socketData <- accept sock
                newThread <- forkIO (serverThunk socketData thunk)
                return ()