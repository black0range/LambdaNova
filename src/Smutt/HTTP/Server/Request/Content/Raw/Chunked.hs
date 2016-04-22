{-# LANGUAGE OverloadedStrings #-}

module Smutt.HTTP.Server.Request.Content.Raw.Chunked  where


import Data.IORef
import Control.Monad.Except
import Control.Concurrent.MVar 
    
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import Network.BufferedSocket (BufferedSocket)
import qualified Network.BufferedSocket as BS


import Smutt.HTTP.Server.Request (Request, ReadState)
import qualified Smutt.HTTP.Server.Request as Req

import Data.Char 
     
import qualified Smutt.Util.Hex as Hex

import Smutt.HTTP.Server.Request.Content.Error

import qualified Smutt.HTTP.Header.Reader as Header
import  Smutt.HTTP.Header (Header)

import Smutt.Util.ByteString
import Data.Monoid

import qualified Data.HashMap.Strict as HS

{-
All chunk headers should be read as soon as a read notices that there are no more bytes left in the current chunk.
-}

-- | Reads part of the request 
readPart :: BufferedSocket -> MVar ReadState -> Int -> ExceptT ContentError IO (Int, ByteString)
readPart bSock rsMVar readLength = do
  readState <- liftIO $ takeMVar rsMVar
  let currentChunkSize = Req.chunkBytesLeft readState
  (bytesRead, message, readState) <-  readPartLoop bSock readState readLength
  liftIO $ putMVar rsMVar readState
  return (bytesRead, message)
  
       

readPartLoop :: BufferedSocket -> ReadState -> Int -> ExceptT ContentError IO (Int, ByteString, ReadState)
readPartLoop bSock readState readLength
  | readLength == 0 = return (0, "", readState)
  | readLength < bytesLeft = readPartOfChunk bSock readState readLength >>= \ (str,rs) -> return (readLength, str, rs)
  | otherwise = do
      (message, firstReadState) <- readChunk bSock readState
      (restLen, nextMessage, secondReadState) <- readPartLoop bSock firstReadState (readLength - bytesLeft)
      return (bytesLeft + restLen, message <> nextMessage, secondReadState)

  where
    bytesLeft = Req.chunkBytesLeft readState 

readPartOfChunk :: BufferedSocket -> ReadState -> Int -> ExceptT ContentError IO (ByteString, ReadState)
readPartOfChunk bSock readState readLength  = do
  message <- liftIO $ BS.readString bSock readLength
  return (message, readState{Req.chunkBytesLeft = Req.chunkBytesLeft readState - readLength})
  

-- | Reads all data available in the request 
readAll :: BufferedSocket -> MVar ReadState -> ExceptT ContentError IO ByteString
readAll bSock rsMVar = do
  readState <- liftIO $ takeMVar rsMVar
  when (Req.chunkedEOF readState) $ liftIO (putMVar rsMVar readState) >> throwError EOF
  
  (message, newReadState) <- readAllLoop bSock readState `catchError` (\e -> liftIO (putMVar rsMVar readState) >> throwError e)
  liftIO $ putMVar rsMVar newReadState
  return message


-- Reads all bytes until EOF is reached
readAllLoop :: BufferedSocket -> ReadState -> ExceptT ContentError IO (ByteString, ReadState)
readAllLoop bSock readState  
  | Req.chunkedEOF readState =  return ("", readState) 
  | otherwise = do
      (message, firstReadState) <- readChunk bSock readState
      (nextMessage, secondReadState) <- readAllLoop bSock firstReadState
      return (message <> nextMessage, secondReadState)

-- | Reads an entire chunk from start to end.
readChunk :: BufferedSocket ->  ReadState ->  ExceptT ContentError IO (ByteString, ReadState)
readChunk bSock readState = do
  chunkMessage <- liftIO $ BS.readString bSock chunkBytesLeft
  nextChunkSize <- readChunkLength bSock 1024
  maybeTrailer <- if nextChunkSize == 0
                  then Just <$> readTrailer bSock readState
                  else return $ Nothing

  let newReadState = readState{
        Req.chunkedTotalRead = Req.chunkedTotalRead readState + chunkBytesLeft ,
        Req.chunkBytesLeft   = nextChunkSize,
        Req.chunkedEOF       = nextChunkSize == 0,
        Req.trailer          = maybeTrailer
        }
  return (chunkMessage, newReadState)
  where
    chunkBytesLeft = Req.chunkBytesLeft readState 
    
 


-- May not actually read the header of the chunked message if there's bytes left in the current mssage
readChunkedHead :: BufferedSocket -> ReadState -> ExceptT ContentError IO ReadState
readChunkedHead bSock readState
  | Req.chunkedEOF readState = throwError EOF
  | chunkBytesLeft > 0 = return readState 
  | otherwise = do
      chunkSize <- readChunkLength bSock 512
      if chunkSize == 0
        then do
        trailer <- readTrailer bSock readState
        return readState{Req.chunkBytesLeft = 0, Req.trailer = Just trailer, Req.chunkedEOF = True}
        else return readState{Req.chunkBytesLeft = chunkSize}

    where
      chunkBytesLeft = Req.chunkBytesLeft readState 


type ChunkSizeRowMaxLength = Int 
-- | Reads the length of a chunk. Limits the length of  
readChunkLength :: BufferedSocket -> ChunkSizeRowMaxLength ->  ExceptT ContentError IO Int
readChunkLength bSock rowMaxSize = do
  maybeLengthLineStr <- liftIO $ BS.readToSequenceLimited bSock crlf rowMaxSize
  case maybeLengthLineStr of
    Nothing ->  throwError ChunkSizeStingTooLarge
    Just lengthLineStr -> let 
      lengthStr = BLC.takeWhile (/=';') $ BLC.dropWhile isSpace lengthLineStr
      maybeLength =  Hex.toIntegral lengthStr 
      in case maybeLength of
        Nothing -> throwError InvalidEncoding
        Just chunkLength -> return chunkLength
    

-- Note to self the temp limit needs to be user decided
readTrailer :: BufferedSocket -> ReadState -> ExceptT ContentError IO Header
readTrailer bSock readState =  do
  header <- liftIO $ runExceptT $ Header.read tempLimit tempLimit bSock
  case header of
    Left _ -> throwError $ CustomError "Chunked trailer error"
    Right hdr -> return hdr
  where
    tempLimit = 1024
  




nonChunkedBodyErrorMsg :: String
nonChunkedBodyErrorMsg = "Tried to read chunked data from non chunked request"
