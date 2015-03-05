{-----------------------------------------------------------------------------------------
Module name: Server Options
Made by:     Tomas Möre 2015
 
------------------------------------------------------------------------------------------}
{-# LANGUAGE OverloadedStrings #-}
module ServerOptions where 
import Data.IORef
{-
data ServerData       = ServerData {  currentConnections         :: IORef Integer
                                    , connectionsFromStart       :: IORef Integer
                                    , startup                    :: Date
                                    , secondsSinceLastConnection :: Integer
                                    }
                                    -}
data ServerSettings  = ServerSettings {  readBufferSize    :: Int                                       
                                        , writeBufferSize  :: Int
                                        , maxConnections   :: Int

                                        , socketKeepAlive  :: Bool
                                        , readTimeout      :: Int
                                        , writeTimeout     :: Int
                                        , keepServing      :: Maybe (IORef Bool)
                                        , maxPathLegnth    :: Int
                                        , maxHeaderLength  :: Int
                                        , maxHeaderCount   :: Int
                                        }

defaultSettings :: ServerSettings
defaultSettings       =  ServerSettings  {  readBufferSize  = (8 * 1024)  -- 8 KB
                                          , writeBufferSize = (8 * 1024)  -- 8 KB
                                          , maxConnections  = 1024
                                          , socketKeepAlive = False
                                          , readTimeout     = 10 * 10 ^ 6
                                          , writeTimeout    = 5 * 10 ^ 6
                                          , keepServing     = Nothing 
                                          , maxPathLegnth   = 1024
                                          , maxHeaderLength = 1024
                                          , maxHeaderCount  = 128
                                          }
