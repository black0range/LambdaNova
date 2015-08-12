{-----------------------------------------------------------------------------------------
Module name: Status Codes
Made by:     Tomas Möre 2015

Utility library. 
------------------------------------------------------------------------------------------}
{-# LANGUAGE OverloadedStrings #-}

module Smutt.Utility.Utility where

import System.IO

import qualified Data.ByteString           as B
import qualified Data.ByteString.Internal  as BI
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Builder   as BB 

import Data.Time.Calendar.WeekDate 
import Data.Time
import Data.Word
import Data.Fixed 
import Data.Monoid
import Data.Bits
import Data.IORef

import Data.Word

import Network.Socket



import Control.Applicative

type ByteString       = B.ByteString


unsafeCond :: [(Bool,a)] -> a 
unsafeCond ((True, a):_) = a
unsafeCond (_: xs) = unsafeCond xs 



builderToStrict :: BB.Builder -> ByteString
builderToStrict = B.concat . BL.toChunks . BB.toLazyByteString 


intToByteString :: Int -> ByteString
intToByteString = builderToStrict . BB.intDec

integerToByteString :: Integer -> ByteString
integerToByteString = builderToStrict . BB.integerDec


byteStringToIntegerReal :: (Num a) => [Word8] -> Maybe a 
byteStringToIntegerReal [] = Just 0 
byteStringToIntegerReal s 
    | 47 < c && c < 59 = fmap adding next 
    | otherwise        = Nothing 
    where 
        (c:cs) = s
        n      = (length s) - 1
        cInt   = fromIntegral c 
        val    = (cInt  - 48) * (10 ^ n) 
        adding = (+val)
        next   = byteStringToIntegerReal cs
        
byteStringToInteger :: (Num a) => ByteString -> Maybe a   
byteStringToInteger s = let unpacked        = B.unpack s
                            firstCharIsDash = (head unpacked) == dash
                            f               = if firstCharIsDash
                                                then 
                                                    negate 
                                                else 
                                                    id 
                            passOn    = if firstCharIsDash
                                            then 
                                                tail unpacked
                                            else 
                                                unpacked
                        in fmap f (byteStringToIntegerReal passOn)
    where 
        dash = (BI.c2w '-')
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

shiftWordTo :: (Bits a, Num a) => [Word8] -> a 
shiftWordTo [] = 0
shiftWordTo (x:xs) = 
    (shift (fromIntegral x) shiftBytes) + shiftWordTo xs 
    where 
        shiftBytes = (length xs) * 8


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





