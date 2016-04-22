{-# LANGUARE OverloadedStrings #-}

module Smutt.HTTP.Server.Request.Content.Raw.Exact (
  readContents
  , readLength    
  ) where

import Smutt.HTTP.Server.Request
import Smutt.HTTP.Server.Request.Content.Error

import Network.BufferedSocket (BufferedSocket)
import qualified Network.BufferedSocket as BS

import qualified Data.ByteString.Lazy.Char8 as BL

import Control.Monad.Except

import Control.Concurrent.MVar

    
 

-- | Reads the entire strictly, us
readContents :: BufferedSocket -> MVar ReadState -> ExceptT ContentError IO BL.ByteString
readContents bSock rsMVar = do
  readState <- liftIO $ takeMVar rsMVar
  
  assureNonEOF rsMVar readState
    
  bodyData <- liftIO $ BS.readString bSock (exactBytesLeft readState)

  liftIO $ putMVar rsMVar readState {
    exactBytesLeft = 0
    }
  liftIO $ const () <$> (BS.readString bSock 2 :: IO BL.ByteString)
  return bodyData


-- | Reads a chunk of a specific size, may return less 
readLength :: BufferedSocket -> MVar ReadState -> Int -> ExceptT ContentError IO (Int, BL.ByteString)
readLength bSock rsMVar readLength = do
  readState <- liftIO $ takeMVar rsMVar
  assureNonEOF rsMVar readState
  
  let
    bytesLeft = exactBytesLeft readState
    willBeEOF = bytesLeft <= readLength
    bytesToRead = if willBeEOF then bytesLeft else  readLength
    
  chunk <- liftIO $  BS.readString bSock bytesToRead

  liftIO $ putMVar rsMVar readState{
    exactBytesLeft = bytesLeft - bytesToRead 
    } 
  when willBeEOF $ const () <$> liftIO (BS.readString bSock 2 :: IO BL.ByteString)
  return (fromIntegral (BL.length chunk), chunk)

      

-- If the request isn't readable (EOF reached)  put the readState back where it belongs and throw an execption
assureNonEOF :: MVar ReadState ->  ReadState -> ExceptT ContentError IO ()
assureNonEOF mvar readState
  | isEOF =  (liftIO $ putMVar mvar readState) >> throwError EOF 
  | otherwise = return ()
  where
    isEOF = exactBytesLeft readState == 0


