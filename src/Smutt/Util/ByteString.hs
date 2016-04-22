{-----------------------------------------------------------------------------------------
Module name: Status Codes
Made by:     Tomas Möre 2015

Utility library.
------------------------------------------------------------------------------------------}
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Smutt.Util.ByteString where


import qualified Data.ByteString           as B
import qualified Data.ByteString.Internal  as BI
import qualified Data.ByteString.Builder   as BB
import qualified Data.ByteString.Lazy as BL
import Data.Word
import Data.Monoid
import Data.Bits

import Data.Fixed
import Data.Time.Calendar.WeekDate 
import Data.Time

import Data.String (IsString)
-- Constants for the newlineCharacters in CCar format
crlf :: (IsString s) => s
crlf = "\r\n"

crlfBuilder :: BB.Builder
crlfBuilder = BB.byteString crlf

chunkedEnd :: B.ByteString
chunkedEnd = "0" <> crlf

builderSemicolon :: BB.Builder
builderSemicolon =  BB.byteString ";"

builderColon::  BB.Builder
builderColon  =  BB.byteString ":"

builderWhiteSpace :: BB.Builder
builderWhiteSpace = BB.byteString " "

-- Removes any unecessary whitespaces from the start and end of a string
stripWhitespace :: B.ByteString -> B.ByteString
stripWhitespace a = let removedFirst      = B.dropWhile BI.isSpaceWord8 a
                        (removedLast, _) = B.spanEnd BI.isSpaceWord8 removedFirst
                    in removedLast


shiftWordTo :: (Bits a, Num a) => [Word8] -> a
shiftWordTo [] = 0
shiftWordTo (x:xs) =
  shift (fromIntegral x) shiftBytes + shiftWordTo xs
    where
        shiftBytes = length xs * 8


getDayName :: Int -> B.ByteString
getDayName 1 = "Mon"
getDayName 2 = "Tue"
getDayName 3 = "Wed"
getDayName 4 = "Thu"
getDayName 5 = "fri"
getDayName 6 = "sat"
getDayName 7 = "sun"

getMonthName :: Int -> B.ByteString
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

getDayOfWeek :: (Integer, Int, Int) -> B.ByteString
getDayOfWeek (_,_,dayNr) =  getDayName dayNr
         


makeHTTPDate :: IO B.ByteString
makeHTTPDate =  do 
                    timeUTC <- getCurrentTime
                    let time                                 = utctDay timeUTC
                        (yearNumber, monthNumber, dayNumber) = toGregorian time 

                        dayName                              = BB.byteString $ getDayOfWeek $ toWeekDate time

                        monthName                            = BB.byteString    $ getMonthName monthNumber

                        tod                                  = localTimeOfDay   $ utcToLocalTime utc timeUTC
                        hour                                 = todHour  tod
                        min                                  = todMin   tod
                        sec                                  = toSeconds $ todSec   tod
 
                        returnString = dayName <>  builderWhiteSpace <> monthName <>  builderWhiteSpace <> (BB.intDec dayNumber) <> builderWhiteSpace <>
                                (BB.intDec hour) <> builderColon <> (BB.intDec min) <> builderColon <> (BB.intDec sec) <> builderWhiteSpace <> (BB.intDec (fromIntegral yearNumber))
                    return $ BL.toStrict $ BB.toLazyByteString returnString
                    where
                      toSeconds = round . fromRational . toRational  :: Pico -> Int 

