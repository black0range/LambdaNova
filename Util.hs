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
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Builder   as BB 

import Data.Time.Calendar.WeekDate 
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.IORef
import Foreign
import Network.Socket
import Data.Word
import Data.Fixed 

import Data.Monoid

type ByteString       = B.ByteString
type BufferSize       = Int
type BufferDataLength = IORef Int
type BufferData       = IORef ByteString

-- This buffered socket is read by first checking the bufferData ByteString. If that is not enought buffer a bit more data... etc
type BufferedSocket   = (Socket, (ForeignPtr Word8), BufferSize, BufferData, BufferDataLength)

builderToStrict :: BB.Builder -> ByteString
builderToStrict = B.concat . BL.toChunks . BB.toLazyByteString 


intToByteString :: Int -> ByteString
intToByteString = builderToStrict . BB.intDec

integerToByteString :: Integer -> ByteString
integerToByteString = builderToStrict . BB.integerDec

intToHex :: Integral a => a -> BB.Builder
intToHex =  BB.word64Hex . fromIntegral 

-- Constants for the newlineCharacters in CCar format
crlf :: ByteString
crlf = "\r\n"

crlfBuilder :: BB.Builder
crlfBuilder = BB.byteString crlf

crlfLength :: Int
crlfLength = 2

chunkedEnd :: ByteString
chunkedEnd = B.concat ["0", crlf]

builderSemicolon :: BB.Builder
builderSemicolon =  (BB.byteString ";") 

builderColon::  BB.Builder
builderColon  =  (BB.byteString ":")

builderWhiteSpace :: BB.Builder
builderWhiteSpace = (BB.byteString " ") 

-- Removes any unecessary whitespaces from the start and end of a string
stripWhitespace :: ByteString -> ByteString
stripWhitespace a = let firstClean      = B.dropWhile BI.isSpaceWord8 a
                        (finalClean, _) = B.spanEnd BI.isSpaceWord8 firstClean
                    in finalClean

picoToSeconds :: Pico -> Int
picoToSeconds a = div' a (10 ^ 12) :: Int

getDayName :: Int -> ByteString
getDayName 1 = "Mon"
getDayName 2 = "Tue"
getDayName 3 = "Wed"
getDayName 4 = "Thu"
getDayName 5 = "fri"
getDayName 6 = "sat"
getDayName 7 = "sun"

getMonthName :: Int -> ByteString
getMonthName 1 = "Jan"
getMonthName 2 = "Feb"
getMonthName 3 = "Mar"
getMonthName 4 = "Apr"
getMonthName 5 = "May"
getMonthName 6 = "Jun"
getMonthName 7 = "Jul"
getMonthName 8 = "Aug"
getMonthName 9 = "Sep"
getMonthName 10 = "Oct"
getMonthName 11 = "Nov"
getMonthName 12 = "Dec"

getDayOfWeek :: (Integer, Int, Int) -> ByteString
getDayOfWeek (_,_,dayNr) =  getDayName dayNr
         





makeHTTPDate :: IO ByteString
makeHTTPDate =  do 
                    timeUTC <- getCurrentTime
                    let time                                 = utctDay timeUTC
                        (yearNumber, monthNumber, dayNumber) = toGregorian time 

                        dayName                              = BB.byteString $ getDayOfWeek $ toWeekDate time

                        monthName                            = BB.byteString    $ getMonthName monthNumber

                        tod                                  = localTimeOfDay   $ utcToLocalTime utc timeUTC
                        hour                                 = todHour  tod
                        min                                  = todMin   tod
                        sec                                  = picoToSeconds $ todSec   tod
 
                        returnString = dayName <>  builderWhiteSpace <> monthName <>  builderWhiteSpace <> (BB.intDec dayNumber) <> builderWhiteSpace <>
                                (BB.intDec hour) <> builderColon <> (BB.intDec min) <> builderColon <> (BB.intDec sec) <> builderWhiteSpace <> (BB.intDec (fromIntegral yearNumber))
                    return $ builderToStrict returnString
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


