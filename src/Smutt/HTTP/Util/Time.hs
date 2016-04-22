{-----------------------------------------------------------------------------------------
Module name: Status Codes
Made by:     Tomas Möre 2015

Utility library.
------------------------------------------------------------------------------------------}
{-# LANGUAGE OverloadedStrings #-}

module Smutt.HTTP.Util.Time where

import Smutt.Util.ByteString

import Data.Time.Calendar.WeekDate
import Data.Time

getDayName :: Int -> a
getDayName 1 = "Mon"
getDayName 2 = "Tue"
getDayName 3 = "Wed"
getDayName 4 = "Thu"
getDayName 5 = "fri"
getDayName 6 = "sat"
getDayName 7 = "sun"

getMonthName :: Int -> a
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

getDayOfWeek :: (Integer, Int, Int) -> a
getDayOfWeek (_,_,dayNr) =  getDayName dayNr



makeHTTPDate :: IO a
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
