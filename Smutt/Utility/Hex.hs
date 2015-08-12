{-----------------------------------------------------------------------------------------
Module name: URL
Made by:     Tomas Möre 2015

The url parser needs work

This library is mean to be imported qualified 
------------------------------------------------------------------------------------------}
{-# LANGUAGE OverloadedStrings #-}
module Smutt.Utility.Hex where

import Data.Monoid 

import Data.Maybe

import Data.Word 
import Data.Bits 

import Foreign.Storable


import Control.Applicative

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI


intToHexCalc :: Word64 ->  [Word8]
intToHexCalc 0 = []
intToHexCalc n =  fromIntegral (n `rem` 16) : intToHexCalc  (n `div` 16)  


nToHexChar:: Word8 -> Word8
nToHexChar x  
    | x < 10  =  x + nrStart
    | otherwise =  (x - 10) + charStart
    where 
        nrStart   = 48
        charStart = 65


numToWord8ListReal :: (Integral a, Bits a) => a  -> Int -> [Word8]
numToWord8ListReal _ (-1) = []
numToWord8ListReal a size = let word = fromIntegral $ shiftR a (size * 8) :: Word8
                                rest = numToWord8ListReal a (size - 1)
                            in ( word:rest) 
numToWord8List:: (Integral a, Bits a, Storable a) => a -> [Word8]
numToWord8List a =  numToWord8ListReal a ((sizeOf a) - 1)

intToHex :: Word64  -> B.ByteString
intToHex =  B.pack . reverse . map nToHexChar . intToHexCalc


hexCharToQuartet :: Word8 -> Maybe Word8 
hexCharToQuartet hexChar  
    | 47 < hexChar && hexChar < 59  = Just $ hexChar - 48 -- 48 is the n of '0'
    | 64 < hexChar && hexChar < 71  = Just $ 10 + (hexChar - 64)
    | 96 < hexChar && hexChar < 103  = Just $ 10 + (hexChar - 96)
    | otherwise                     = Nothing  

-- For ansii only
byteStringToHexReal :: Word32 -> [Word8] -> Maybe Int
byteStringToHexReal _ [] = Just 0
byteStringToHexReal index (currentChar:rest) = do 
    hexWord8 <- (fromIntegral <$> (hexCharToQuartet currentChar)) 
    nextStep <- byteStringToHexReal nextIndex rest 
    return $ nextStep +  (fromIntegral $ hexWord8 * (16 ^ index))

    where nextIndex = (index - 1)

byteStringToHex :: B.ByteString -> Maybe Int 
byteStringToHex str = let len = B.length str 
                      in byteStringToHexReal (fromIntegral len) $ B.unpack str

quartetPairToByte ::Word8 -> Word8 -> Word8 
quartetPairToByte firstQuartet secondQuartet = (shift firstQuartet 4) `xor` secondQuartet

hexToBytes :: [Word8] -> Maybe [Word8] 
hexToBytes [] = Just []
hexToBytes (_:[]) = Nothing 
hexToBytes (firstChar:secondChar:rest) = do 
    firstQuartet    <- hexCharToQuartet firstChar
    secondQuartet   <- hexCharToQuartet secondChar
    nextStep        <- hexToBytes rest
    let byte = quartetPairToByte firstQuartet secondQuartet
    return (byte:nextStep)
