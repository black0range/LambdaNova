{-----------------------------------------------------------------------------------------
Module name: LambdaNova - a web server
Made by:     Tomas Möre 2014


Usage:  Pass a function of type (HTTPRequest -> IO Response) to the |serve| fucntion exported by module
        yor function is now responsible for putting together the response.
        An empty response will be treated as a |200 OK| without any content 
    
        This library uses STRICT strings internally. 



Notes for editor: Many functions are splitted into two parts, Any function with the postfix "Real"
                  has a initating function with the same name but without the postfix

------------------------------------------------------------------------------------------}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server  
( serve
) where  


import Data.Time.Clock

import Control.Concurrent
import Control.Monad
import qualified Control.Exception as E 

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

import qualified ErrorResponses as ERROR

import Util
import HTTP
import StatusCodes
import qualified Headers as H 

type WebThunk = (HTTPRequest -> IO Response)

type KeepGoing = Bool




mainHTTPVersion:: ByteString
mainHTTPVersion = "HTTP/1.1"

testResponse:: ByteString
testResponse = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"

invalidReuqestResponse :: ByteString
invalidReuqestResponse = B.append "HTTP/1.1 400 Bad Request" crlf




-- This function handles the response to the request. Return a Boolean. 
showExcept a = putStrLn $ "Exception: " ++ show a 

responseHandler :: (ByteString -> IO Int) -> HTTPParsingResult -> WebThunk -> IO KeepGoing
responseHandler send (ParsingSuccess request) thunk = 
  do
    response <- (thunk request) `E.catches` [ E.Handler (\ (ex :: E.ErrorCall )     -> showExcept ex >> ERROR.internalServerError)
                                            , E.Handler (\ (ex :: E.IOException)    -> showExcept ex >> ERROR.internalServerError)
                                            , E.Handler (\ (ex :: E.ArithException) -> showExcept ex >> ERROR.internalServerError)]
    dateHeader <- H.dateHeader                                            
    --response <- E.catch (thunk request)  (\ e -> putStrLn "Error!!!" >>= (\ _ -> ERROR.internalServerError))
    -- Date header for responses 
   
    let (status, responseHeaders) = case response of 
                                        FullResponse      a b  _   -> (a, b)
                                        FullLazyResponse  a b  _ _ -> (a, b)
                                        ChukedResponse    a b  _   -> (a, b)

        statusLine                = B.concat [mainHTTPVersion, " ", statusCodeToStr status, crlf]
        keepAlive                 = case lookup H.Connection responseHeaders of 
                                            Nothing      -> return True 
                                            Just _       -> return False
                                            _            -> return True
                                                    

        headerSender :: [ByteString] -> IO ()
        headerSender a = if null a
                            then  
                                send crlf >> return () 
                            else  
                                let chunk = take 10 a
                                    rest  = drop 10 a
                                    readString = B.concat chunk
                                in (send readString) >> headerSender rest

        dataSender :: ByteString -> IO ()
        dataSender "" = return ()
        dataSender a = let  chunkSize = (1024 * 4) 
                            chunk = B.take chunkSize a
                            rest  = B.drop chunkSize a
                        in send chunk >> dataSender rest

        lazyDataSender :: [ByteString] -> IO ()
        lazyDataSender [] = return ()
        lazyDataSender (x:xs) =  dataSender x >> lazyDataSender xs

        chunkedSender :: [ByteString] -> IO ()
        chunkedSender []     = void $ send chunkedEnd -- hex zero
        chunkedSender (x:xs) =  let chunkSize = B.length x
                                in  (send $ builderToStrict $ (intToHex chunkSize) <> builderSemicolon<> crlfBuilder) >>
                                        send x >>
                                            chunkedSender xs 
    case response of 
        -- If response is set to Manual we expect the user to have taken care of everything
        ManualResponse -> return False
        -- FullResposne asks the server to count the content and send it all. 
        -- Full response takes a full bytestring however we still send the data in smaller sizes of max 4kB each (4 KB will be set to a setting)
        FullResponse   _ _ fullString    -> do
                                                let contentLength = (H.ContentLength, intToByteString $ B.length fullString)
                                                    headers       = (contentLength : dateHeader : responseHeaders) 
                                                    headerList    = map H.toSendRow headers

                                                send statusLine >> 
                                                    headerSender headerList >> 
                                                        dataSender fullString >> 
                                                            send crlf >>
                                                                keepAlive
        -- Full LazyResponbse is the same ass FullResponse Except that we expect a list of ByteStrings
        -- A Full Lazy Response requires a content Length parameter as well
        FullLazyResponse _ _ chunkedString contentLengthIn -> do 
                                                                let contentLength = (H.ContentLength , integerToByteString $ fromMaybe (fromIntegral (sum $ map B.length chunkedString)) contentLengthIn )
                                                                    headers       = (contentLength : dateHeader : responseHeaders)  
                                                                    headerList    = map H.toSendRow headers

                                                                send statusLine  >>
                                                                    headerSender headerList >>
                                                                        lazyDataSender chunkedString>>
                                                                            send crlf >>
                                                                                keepAlive
        -- ChunkedResponse is a response where we use chunks to send data. This means that for every Byrestring in the list we send one cunk.
        -- This will only work on http/1.1 clients 
        ChukedResponse _ _ chunkedString -> do  
                                                let headers       = ( H.chunkedHeader : dateHeader : responseHeaders)
                                                    headerList    = map H.toSendRow headers
                                                send statusLine >>
                                                    headerSender headerList >>
                                                        chunkedSender chunkedString >>
                                                            send crlf >>
                                                                keepAlive
responseHandler s error _ = 
  let status = case error of 
                  URLTooLarge        -> statusCodeToStr 414 --" 414 Request-URI Too Long\n\r"
                  InvalidVersion     -> statusCodeToStr 505 --" 505 HTTP Version Not Supported\n\r"
                  InvalidMethod      -> statusCodeToStr 501 --" 405 Not Implemented\n\r"        
                  HeaderLimitReached -> statusCodeToStr 413 --" 413 Request Entity Too Large\n\r"
                  InvalidRequestLine -> statusCodeToStr 400 --" 400 Bad Request\n\r"
                  LengthRequired     -> statusCodeToStr 411 --" 411 Length Required\n\r"
                  _                  -> statusCodeToStr 400 --" 400 Bad Request\n\r"
      response = B.concat [mainHTTPVersion, " ", status, crlf, crlf]
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
        request      <- readHTTPRequst bSocket bufferSize send
        keepAlive <- responseHandler send request thunk
        putStrLn $ "keep alive?: "++ show keepAlive
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