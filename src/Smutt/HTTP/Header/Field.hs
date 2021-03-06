{-----------------------------------------------------------------------------------------
Module name: Header.Converter
Made by:     Tomas Möre 2015

Conatins functions to convert one header format to another

------------------------------------------------------------------------------------------}


{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DefaultSignatures #-}
module Smutt.HTTP.Header.Field (parse) where


import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL

import qualified Data.Text.Lazy.Encoding as TEnc
import qualified Data.Text.Encoding.Error as TEnc


  
-- | Encoding is supposed to be in ascii format. But this accepts UTF8 aswell.
-- If there's any encoding errors we let them through and replace them with the UTF8 replacement character (�)
-- Note that the field names are all converted to lowercase 
parse :: BL.ByteString -> Maybe (Text, Text)
parse inStr = if value == ""
              then Nothing
              else Just (TL.toLower $ decode $ BL.dropWhile (==' ') name,
                         decode (BL.dropWhile (==' ') $ BL.drop 1 value)) 
  where
    (name, value) = BL.break (==':') inStr
    decode = TEnc.decodeUtf8With $ TEnc.replace '�'



