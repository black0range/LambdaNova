{-----------------------------------------------------------------------------------------
Module name: Server Options
Made by:     Tomas Möre 2015
 
------------------------------------------------------------------------------------------}


{-# LANGUAGE OverloadedStrings #-}
data ServerData       = ServerData {  currentConnections         :: IORef Integer
									, connectionsFromStart 		 :: IORef Integer
									, startup              		 :: Date
									, secondsSinceLastConnection :: Integer
									}
data ServerOptions    = ServerOptions {   readBufferSize  :: Integer
										, readBufferSize  :: Integer 
										, writeBufferSize :: Integer
										, maxConnections  :: Integer
										, keepAlive       :: Boolean
										, readTimeout     :: Int
										, writeTimeout    :: Int
										, extraData       :: Maybe IORef 

										}

defaultServerOptions :: ServerOptions
defaultServerOptions = ServerOptions {    readBufferSize  = (1024 * 4)
										, readBufferSize  = (1024 * 4)
										, writeBufferSize = (1024 * 4)
										, maxConnections  = (1024)
										, keepAlive       :: Boolean
										, readTimeout     :: Int
										, writeTimeout    :: Int
										, extraData       :: Maybe IORef 

										}