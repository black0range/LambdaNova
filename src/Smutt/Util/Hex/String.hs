{-----------------------------------------------------------------------------------------
Module name: Hex.String
Made by:     Tomas Möre 2015


Handles conversion between any kind of string to the Integral value and vice versa
------------------------------------------------------------------------------------------}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
--module Smutt.Util.Hex.String where
module Smutt.Util.Hex.String (HexString, toIntegral, fromIntegral) where

import Prelude hiding(fromIntegral)
import qualified Prelude as Prelude (fromIntegral)
import Data.Bits
import Data.List
import Data.Word

import qualified Smutt.Util.Hex.Char as HC
import qualified Data.ByteString.Internal as BI

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

class HexString s where
  toIntegral :: (Integral a, Bits a) => s -> Maybe a
  fromIntegral :: (Integral a, Bits a) => a -> s


-- ByteStrings

instance HexString B.ByteString where
  toIntegral   = fromByteString
  fromIntegral = toHexByteString


fromByteString :: (Integral a,  Bits a) => B.ByteString -> Maybe a
fromByteString "" = Nothing
fromByteString str = sumHexString lastIndex 0
  where
    lastIndex = B.length str - 1
    sumHexString :: (Integral a, Bits a) => Int -> a -> Maybe a
    sumHexString (-1) sum = Just sum
    sumHexString n sum = do
      let currentIndex = lastIndex - n
      currentValue <- (16 ^ n *) <$> HC.toIntegral (B.index str currentIndex)
      let newSum = sum + currentValue
      sumHexString (n - 1) newSum

toHexByteString :: (Integral a, Bits a)  => a -> B.ByteString
toHexByteString = B.pack . (toHexList :: (Integral a, Bits a) => a -> [Word8])

-- ByteString Lazy

instance HexString BL.ByteString where
  toIntegral   =  fromByteStringLazy
  fromIntegral =  BL.fromStrict <$> fromIntegral

fromByteStringLazy :: (Integral a, Num a, Bits a) => BL.ByteString -> Maybe a
fromByteStringLazy lazyStr = do
  valueList <- sequence maybeHexValueList
  Just $ sum $ zipWith (\ exponent value -> 0x10 ^ exponent * value) multipliers valueList
  where
    reversedStr       = reverse $ BL.toChunks lazyStr
    hexStrings        = filter (not . B.null) reversedStr
    maybeHexValueList = map fromByteString hexStrings ::  (Integral a, Bits a) => [Maybe a]
    lengths           = map B.length hexStrings
    multipliers       = 0 : lengths





-- Strings

instance HexString [Char] where
  toIntegral = fromHexString
  fromIntegral   = toHexString

toHexString :: (Integral a, Bits a)  => a -> String
toHexString n = map BI.w2c quartetList
  where
    quartetList = toHexList n :: [Word8]


fromHexString :: (Integral a, Bits a) => String -> Maybe a
fromHexString str = do
  charList <- sequence $ reverse $ map HC.toIntegral str :: (Integral a, Bits a) => Maybe [a]
  Just $ foldr (\ hexCharValue sum -> shiftL sum 4 + hexCharValue) 0  charList


-- Text
instance HexString T.Text where
  toIntegral = toIntegral . T.unpack
  fromIntegral   = T.pack . fromIntegral

-- Text Lazy
instance HexString TL.Text where
  toIntegral = toIntegral . TL.unpack
  fromIntegral   = TL.pack . fromIntegral


-- Util / general

toHexList :: (Integral a, Bits a, Num a, HC.HexChar c) => a -> [c]
toHexList = reverse . unfoldr (\ n ->
  if n == 0
    then Nothing
    else Just (lastQuartet n, shiftR n 4))
  where
    lastQuartet =  HC.fromIntegralUnsafe . (0xf .&.) :: (Num a, Bits a, Integral a, HC.HexChar c) => a -> c
