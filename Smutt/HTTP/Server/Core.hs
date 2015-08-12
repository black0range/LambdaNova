{-----------------------------------------------------------------------------------------
Module name: Smutt- a web server
Made by:     Tomas Möre 2014


Usage:  Pass a function of type (HTTPRequest -> IO Response) to the |serve| fucntion exported by module
        yor function is now responsible for putting together the response.
        An empty response will be treated as a |200 OK| without any content 
    
        This library uses STRICT strings internally. 



Notes for editor: Many functions are splitted into two parts, Any function with the postfix "Real"
                  has a initating function with the same name but without the postfix

------------------------------------------------------------------------------------------}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Smutt.HTTP.Server.Core where  


import Data.Time.Clock

import Control.Concurrent
import Control.Monad
import qualified Control.Exception as E 

import System.IO
import System.IO.Error


import Network.Socket

import qualified Data.ByteString           as B  hiding (pack, putStrLn)
import qualified Data.ByteString.Char8     as B  hiding (findSubstring, elemIndex, split, break, spanEnd, dropWhile)

import qualified Data.ByteString.Internal  as BI
import qualified Data.ByteString.Builder   as BB
import qualified Data.ByteString.Lazy      as BL


import Data.Word

import Data.IORef
import Data.Maybe
import Data.Either
import Data.Monoid
import System.IO.Unsafe 

import qualified Smutt.HTTP.Server.ErrorResponses as ERROR


import Smutt.Utility.Hex
import Smutt.Utility.Utility
import Smutt.HTTP.StatusCodes
import Smutt.HTTP.Server.Settings

import qualified Network.BufferedSocket as BS 
import qualified Smutt.HTTP.Headers as H 

import qualified Smutt.HTTP.Server.Request as HTTP
import qualified Smutt.HTTP.Server.Response as HTTP
import qualified Smutt.HTTP.Common as HTTP 


import Smutt.Utility.TCPServer 

type WebThunk = (HTTP.Request -> IO HTTP.Response)

type KeepGoing = Bool




mainHTTPVersion:: ByteString
mainHTTPVersion = "HTTP/1.1"

testResponse:: ByteString
testResponse = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"

invalidReuqestResponse :: ByteString
invalidReuqestResponse = "HTTP/1.1 400 Bad Request" <> crlf




-- This function handles the response to the request. 
showExcept a = putStrLn $ "Exception: " ++ show a 

responseHandler :: BS.BufferedSocket -> HTTP.ParsingResult -> WebThunk -> ServerSettings ->IO KeepGoing

responseHandler bSocket (HTTP.ParsingSuccess request) thunk settings = 
  do
    responseIN <- (thunk request) `E.catches` [ E.Handler (\ (ex :: E.ErrorCall )     -> showExcept ex >> ERROR.internalServerError)
                                              , E.Handler (\ (ex :: E.IOException)    -> showExcept ex >> ERROR.internalServerError)
                                              , E.Handler (\ (ex :: E.ArithException) -> showExcept ex >> ERROR.internalServerError)]
    dateHeader <- unsafeInterleaveIO $ H.dateHeader                                            
    --response <- E.catch (thunk request)  (\ e -> putStrLn "Error!!!" >>= (\ _ -> ERROR.internalServerError))
    -- Date header for responses 
   
    let 
        send = BS.send bSocket :: (ByteString -> IO ())
        -- A security check to make sure that the response wont be sent as chunked if the HTTP version isn't able to handle it
        -- A warning to the developer should be that if the chunked encoding is sent and the HTTP version isn't correct this might take up a lot of ram
        response = if (HTTP.requestVersion request) < HTTP.HTTP11 
                        then
                            case responseIN of 
                                HTTP.ChunkedResponse s h b -> HTTP.FullLazyResponse s h b Nothing
                                _                    -> responseIN
                        else
                            responseIN 

        httpVersionString = HTTP.versionToString (HTTP.requestVersion request)

        (status, responseHeaders) = case response of 
                                        HTTP.FullResponse      a b  _    -> (a, b)
                                        HTTP.FullLazyResponse  a b  _ _  -> (a, b)
                                        HTTP.ChunkedResponse   a b  _    -> (a, b)
                                        HTTP.HeadersResponse   a b       -> (a, b)

        statusLine                = B.concat [httpVersionString, " ", statusCodeToStr status, crlf]
        keepAlive                 = case response of 
                                        HTTP.NoResponse     -> False 
                                        HTTP.ManualResponse -> False 
                                        _ -> case lookup H.Connection responseHeaders of 
                                                Nothing      ->  True 
                                                Just _       ->  False
                                                    

        headerSender :: [ByteString] -> IO ()
        headerSender [] =   send crlf >> return () 
        headerSender a  =   let chunk = take 16 a
                                rest  = drop 16 a
                                readString = B.concat chunk
                            in (send  readString) >> headerSender rest

        dataSender :: ByteString -> IO ()
        dataSender a = 
            do
                let chunkSize = (writeBufferSize settings)
                    chunk     = B.take chunkSize a
                    rest      = B.drop chunkSize a
                send chunk  
                if B.length rest > 0 
                    then dataSender rest
                    else return ()
                

        lazyDataSender :: [ByteString] -> IO ()
        lazyDataSender [] = return ()
        lazyDataSender (x:xs) =  dataSender x >> 
                                    lazyDataSender xs

        chunkedSender :: [ByteString] -> IO ()
        chunkedSender []     = void $ send chunkedEnd -- hex zero
        chunkedSender (x:xs) =  let chunkSize = B.length x
                                    hexSize   = intToHex (fromIntegral chunkSize)
                                in  (send hexSize) >>
                                        send x >>
                                            send crlf >>
                                                chunkedSender xs 


    case response of 
        -- If response is set to Manual we expect the developer to have taken care of everything
        HTTP.NoResponse     -> return () 
        HTTP.ManualResponse -> return ()
        -- FullResposne asks the server to count the content and send it all. 
        -- Full response takes a full bytestring however we still send the data in smaller sizes of max 4kB each (4 KB will be set to a setting)
        HTTP.FullResponse   _ _ fullString    -> 
            do
                let contentLength = (H.ContentLength, intToByteString $ B.length fullString)
                    headers       = (contentLength : dateHeader : responseHeaders) 
                    headerList    = map H.toSendRow headers

                send statusLine 
                headerSender headerList
                dataSender fullString
                
        -- Full LazyResponbse is the same ass FullResponse Except that we expect a list of ByteStrings
        -- A Full Lazy Response requires a content Length parameter as well
        HTTP.FullLazyResponse _ _ chunkedString contentLengthIn -> 
            do 
                let contentLength = (H.ContentLength , integerToByteString $ fromMaybe (fromIntegral (BL.length chunkedString)) contentLengthIn )
                    headers       = (contentLength : dateHeader : responseHeaders)  
                    headerList    = map H.toSendRow headers

                send statusLine 
                headerSender headerList
                lazyDataSender (BL.toChunks chunkedString)
                
        -- ChunkedResponse is a response where we use chunks to send data. This means that for every Byrestring in the list we send one cunk.
        -- This will only work on http/1.1 clients 
        HTTP.ChunkedResponse _ _ chunkedString -> 
            do  
                let headers       = ( H.chunkedHeader : dateHeader : responseHeaders)
                    headerList    = map H.toSendRow headers
                send statusLine
                headerSender headerList
                chunkedSender (BL.toChunks chunkedString)
                send crlf
                
        HTTP.HeadersResponse _ _              ->
            do 
                let headers     = (dateHeader:responseHeaders)
                    headerList  = map H.toSendRow headers
                send statusLine 
                headerSender headerList

    -- Reads any remaining data from the request and throws it away, in case the server din't read it
    when keepAlive $ HTTP.readToEndOfRequest (HTTP.requestBodyReader request) (tcpChunkMaxSize settings)

    return keepAlive


responseHandler _ HTTP.ClientQuit _ _ = return False 
responseHandler bSocket error _ _= 
  let status = case error of 
                  HTTP.QueryInvalidlyPercentEncoded  -> statusCodeToStr 400
                  HTTP.URLTooLarge        -> statusCodeToStr 414 --" 414 Request-URI Too Long\n\r"
                  HTTP.InvalidVersion     -> statusCodeToStr 505 --" 505 HTTP Version Not Supported\n\r"
                  HTTP.InvalidMethod      -> statusCodeToStr 501 --" 405 Not Implemented\n\r"        
                  HTTP.HeaderLimitReached -> statusCodeToStr 413 --" 413 Request Entity Too Large\n\r"
                  HTTP.InvalidRequestLine -> statusCodeToStr 400 --" 400 Bad Request\n\r"
                  HTTP.LengthRequired     -> statusCodeToStr 411 --" 411 Length Required\n\r"
                  _                       -> statusCodeToStr 400 --" 400 Bad Request\n\r"

      response = B.concat [mainHTTPVersion, " ", status, crlf, crlf]
  in do 
      BS.send bSocket response
      return False


-- The server function, Currently give it a function that will process the request and it will run a new thread from every request
serveWithSettings :: WebThunk -> ServerSettings-> IO ()
serveWithSettings thunk serverSettings = makeTCPServer (webServerThunk thunk serverSettings) serverSettings

-- The main thunk of the server. This code is resposible for reading the request handing it over to the "real thunk"
-- then sending the response in an appropiate manner along with closing down the socket and freeing the pointer
webServerThunk :: WebThunk -> ServerSettings -> BS.BufferedSocket -> IO ()
webServerThunk thunk settings bSocket = 
    do  let readLoop = do 
                        request      <- HTTP.readRequest bSocket settings `E.catch` (\ e -> if isEOFError e then return HTTP.ClientQuit else E.throw e ) 
                        keepAlive    <- responseHandler bSocket request thunk settings

                        
                        BS.flush bSocket
                        
                        if and [  keepAlive 
                                , not $ HTTP.connectionClosed request 
                                , HTTP.protocolCanKeepAlive request]
                          then do 
                                continue <- BS.waitForRead bSocket (readTimeout settings)
                                if  continue 
                                    then readLoop
                                    else return ()
                          else 
                            return ()
         
        readLoop




-- mostly just a test. 
serve thunk = serveWithSettings thunk defaultSettings






