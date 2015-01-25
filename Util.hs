{-----------------------------------------------------------------------------------------
Module name: Status Codes
Made by:     Tomas Möre 2015

Utility library. 
------------------------------------------------------------------------------------------}
{-# LANGUAGE OverloadedStrings #-}

module Util where

import System.IO
import qualified Data.CaseInsensitive      as CI
import qualified Data.ByteString           as B
import qualified Data.ByteString.Internal  as BI
import Data.IORef
import Foreign
import Network.Socket
import Data.Word


type ByteString       = B.ByteString
type BufferSize       = Int
type BufferDataLength = IORef Int
type BufferData       = IORef ByteString

-- This buffered socket is read by first checking the bufferData ByteString. If that is not enought buffer a bit more data... etc
type BufferedSocket   = (Socket, (ForeignPtr Word8), BufferSize, BufferData, BufferDataLength)


-- Constants for the newlineCharacters in CCar format
crlf :: ByteString
crlf = "\r\n"

crlfLength :: Int
crlfLength = 2

-- Removes any unecessary whitespaces from the start and end of a string
stripWhitespace :: ByteString -> ByteString
stripWhitespace a = let firstClean      = B.dropWhile BI.isSpaceWord8 a
                        (finalClean, _) = B.spanEnd BI.isSpaceWord8 firstClean
                    in finalClean


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


