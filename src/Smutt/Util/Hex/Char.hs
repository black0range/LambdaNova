{-----------------------------------------------------------------------------------------
Module name: Hex.Char
Made by:     Tomas Möre 2015

Handles conversion between ASCCI envoded hex characters to Integral values and vice versa.

------------------------------------------------------------------------------------------}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}

module Smutt.Util.Hex.Char (HexChar, toIntegral, toIntegralUnsafe, fromIntegral, fromIntegralUnsafe) where

import qualified Smutt.Util.Ascii as Ascii

import Prelude hiding(fromIntegral)
import qualified Prelude as Prelude (fromIntegral)
import Data.Char hiding (isNumber)
import Data.Word
import Data.Int
import Data.Bits


class HexChar c where
  toIntegral         :: (Integral a, Bits a) => c -> Maybe a
  toIntegralUnsafe   :: (Integral a, Bits a) => c -> a
  fromIntegral       :: (Integral a, Bits a) => a -> Maybe c
  fromIntegralUnsafe :: (Integral a, Bits a) => a -> c

  default toIntegral :: (Integral a, Integral c) => c -> Maybe a
  toIntegral = integralFromASCIIValue

  default toIntegralUnsafe :: (Integral a, Integral c) => c -> a
  toIntegralUnsafe = Ascii.digitToIntegral

  default fromIntegral :: (Integral c, Integral i) => i -> Maybe c
  fromIntegral = toASCIIValue

  default fromIntegralUnsafe :: (Integral c, Integral i, Bits i) => i -> c
  fromIntegralUnsafe = Ascii.integralToDigit

-- The
instance HexChar Char where
  toIntegral         = integralFromASCIIValue . ord
  toIntegralUnsafe   = Ascii.digitToIntegral . ord
  fromIntegral i     =  chr <$> toASCIIValue i
  fromIntegralUnsafe = chr . (Ascii.integralToDigit :: ((Integral a) =>a -> Int))

  -- Geeneral
  -- Should cover all types of char conversion except the Char datatype
instance HexChar Word where
instance HexChar Word8 where
instance HexChar Word16 where
instance HexChar Word32 where
instance HexChar Word64 where

instance HexChar Int where
instance HexChar Int8 where
instance HexChar Int16 where
instance HexChar Int32 where
instance HexChar Int64 where
instance HexChar Integer where
-- Convers a ascii or ascii "compatible" value to a hexCharacter
-- (0-9, a-f) ()
integralFromASCIIValue :: (Integral i, Integral c) => c -> Maybe i
integralFromASCIIValue c = if Ascii.isHexDigit c
  then Just (Ascii.digitToIntegral c)
  else Nothing

toASCIIValue :: (Integral c, Integral i) => i -> Maybe c
toASCIIValue c = if 0 <= c || c <= 15
  then Just (Ascii.integralToDigit c)
  else Nothing
