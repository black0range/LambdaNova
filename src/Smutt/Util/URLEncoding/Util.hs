{-----------------------------------------------------------------------------------------
Module name: URLEncoding.ByteString
Made by:     Tomas Möre 2015


------------------------------------------------------------------------------------------}
{-# LANGUAGE OverloadedStrings #-}
module Smutt.Util.URLEncoding.Util where


import Smutt.Util.Char
import Data.Word
import Data.Monoid 
import Data.ByteString as B

import Smutt.Util.Hex.String as HexString
import Smutt.Util.Ascii as Ascii

encodedWord8 :: Word8 -> Maybe B.ByteString
encodedWord8 c
  | c == fromChar ' ' = Just "+"
  | Ascii.isAlphaNum c || isAllowedChar c = Nothing
  | otherwise = Just $ "%" <> HexString.fromIntegral c

isAllowedChar :: (IsChar c) => c -> Bool
isAllowedChar c = c == fromChar '-' || c == fromChar '.' || c == fromChar '_' || c == fromChar '~'
