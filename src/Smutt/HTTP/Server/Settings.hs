{-----------------------------------------------------------------------------------------
Module name: ServerSettings
Made by:     Tomas Möre 2016
------------------------------------------------------------------------------------------}



module Smutt.HTTP.Server.Settings where
import Data.IORef
import Network.Socket


{-|
These are the server settings for the Smutt server.

Explanation of the server fiends are all follow.

ReadBufferSize :: The size of the buffer that the buffered socket keeps

writeBufferSize :: The size of the writing buffer that the buffered socket keeps

Maxonnections :: The max number of the connections the server may have at any time

Port :: The port on which the server will run on

keepAlive :: If TCP keep alive should be on or off

readTimeout :: How long the server will wait before data is recieved if the timeout runns out. If it does the various parts will throw
varous errors. If nothing there are no timeout (potetially dangerous)

KeepServing :: An ugly hack to shut down the main server. If this has been called the children processes may still be running. 


MaxRequestLineLength :: The max length in bytes that the Request-line may be

MaxHeaderLength :: Max size in bytes that the header may be

MaxHeaderLineLength :: Max number bytes in one line of the header. (Remove maybe?)


MaxBodySize :: The max length that a body may be, This is ignored for chunked data INVLUSIVE!
 


PROPOSITIONS

Unchunk :: A way to allow the webserver to unchunk any chunked data... I don't like it but some people might? 

|-}

data ServerSettings  = ServerSettings {   readBufferSize      :: Int
                                        , writeBufferSize     :: Int

                                        , maxConnections      :: Int
                                        , port                :: PortNumber
                                        , keepAlive           :: Bool
                                        , readTimeout         :: Maybe Int
                                                                 
                                        , writeTimeout        :: Maybe Int
                                                                 
                                        , keepServing         :: Maybe (IORef Bool)

                                        , maxRequestLineLength :: Int
                                        , maxHeaderLength      :: Int
                                        , maxHeaderLineLength  :: Int

                                        , maxChunkedHeaderSize :: Int
                                        , maxChunkedTrailerSize :: Int


                                        , socketKeepAlive :: Bool 
                                        

                                        }


-- | default settings contains arbitary default settings for the webserver
defaultSettings :: ServerSettings
defaultSettings =  ServerSettings  {  readBufferSize  = (8 * 1024)  -- 8 KiB
                                   , writeBufferSize = (8 * 1024)  -- 8 KiB
                                     
                                   , maxConnections  = 1024
                                   , port            = 8000
                                   , keepAlive       = False
                                   , readTimeout     = Just (10 * 10 ^ 6)
                                   , writeTimeout    = Just (5 * 10 ^ 6)
                                   , keepServing     = Nothing
                                     
                                   , maxRequestLineLength = 1024
                                   , maxHeaderLength   = 2048
                                   , maxHeaderLineLength   = 256
                                     
                                   , maxChunkedHeaderSize = 512
                                   , maxChunkedTrailerSize = 1024 * 2
                                     
                                   , socketKeepAlive = False

                                          }
