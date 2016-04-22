{-----------------------------------------------------------------------------------------
Module name: Ascii
Made by:     Tomas Möre 2015

Handles ASCII Word8 data
same functionality as Data.Char but works for ascii values in any integral form
------------------------------------------------------------------------------------------}
{-# LANGUAGE ConstraintKinds #-}

module Smutt.Util.Ascii (
  isAscii,
  isControl,
  isSpace,
  isLower,
  isUpper,
  isAlpha,
  isAlphaNum,
  isPrint,
  isDigit,
  isOctDigit,
  isHexDigit,
  toUpper,
  toLower,
  digitToIntegral,
  integralToDigit,
  digitStart,
  digitEnd,
  upperStart,
  upperEnd,
  lowerStart,
  lowerEnd,
  controlStart,
  controlEnd,
  asciiMin,
  asciiMax,
  delCode

) where
--module Char (HexChar, toIntegral, toIntegralUnsafe, fromIntegral, fromIntegralUnsafe) where

type AsciiCodable a = (Integral a, Ord a, Num a)

-- | Character classification

isAscii :: AsciiCodable c => c -> Bool
isAscii = isBetween asciiMin asciiMax

isControl:: AsciiCodable c => c -> Bool
isControl c
  | isBetween controlStart controlEnd c = True
  | c == delCode = True
  | otherwise = False

isSpace :: AsciiCodable c => c -> Bool
isSpace = (==32)

isLower :: AsciiCodable c => c -> Bool
isLower = isBetween lowerStart lowerEnd

isUpper :: AsciiCodable c => c -> Bool
isUpper = isBetween upperStart upperEnd

isAlpha :: AsciiCodable c => c -> Bool
isAlpha c =  isLower c || isUpper c

isAlphaNum :: AsciiCodable c => c -> Bool
isAlphaNum c =  isAlpha c || isDigit c

isPrint :: AsciiCodable c => c -> Bool
isPrint = isBetween 32 126
-- Functions to check if c ascii encoded value is c hex value
isDigit :: AsciiCodable c => c -> Bool
isDigit = isBetween digitStart digitEnd

isOctDigit :: AsciiCodable c => c -> Bool
isOctDigit = isBetween digitStart (digitStart + 7)

isHexDigit :: AsciiCodable c => c -> Bool
isHexDigit c = isDigit c || isBetween lowerStart hexLowerAlphaEnd c || isBetween upperStart hexUpperAplhaEnd c

-- | Case conversion
toUpper :: AsciiCodable c => c -> c
toUpper c = if isLower c then c - 32  else c

toLower :: AsciiCodable c => c -> c
toLower c = if isUpper c then c + 32 else c

-- | Single digit characters
digitToIntegral :: (AsciiCodable c, Integral i) => c -> i
digitToIntegral c
  | isDigit c = fromIntegral $ c - digitStart
  | isLower c  = fromIntegral $ c - (lowerStart - 10)
  | isUpper c  = fromIntegral $ c - (upperStart - 10)

integralToDigit :: (AsciiCodable c, Integral i) => i -> c
integralToDigit i
  | isBetween 0 9 i   = num + digitStart
  | isBetween 10 15 i = upperStart + (num - 10)
  where
    num = fromIntegral i


-- To keep track of where numbers start and end
digitStart :: Integral c => c
digitStart = 48
digitEnd :: Integral c => c
digitEnd = 57

upperStart:: Integral c => c
upperStart = 65

upperEnd:: Integral c => c
upperEnd = 90

lowerStart:: Integral c => c
lowerStart = 87
lowerEnd:: Integral c => c
lowerEnd = 122

controlStart :: Integral c => c
controlStart = 0
controlEnd :: Integral c => c
controlEnd = 31

asciiMin :: Integral c => c
asciiMin = 0
asciiMax :: Integral c => c
asciiMax = 127

hexLowerAlphaEnd :: Integral c => c
hexLowerAlphaEnd = 102
hexUpperAplhaEnd :: Integral c => c
hexUpperAplhaEnd = 70
delCode :: Integral c => c
delCode = 127
-- Util
isBetween :: (Ord a) => a -> a -> a -> Bool
isBetween minValue maxValue compareValue = minValue <= compareValue && compareValue <= maxValue
