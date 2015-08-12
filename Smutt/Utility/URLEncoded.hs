{-----------------------------------------------------------------------------------------
Module name: URL
Made by:     Tomas Möre 2015

The url parser needs work

This library is mean to be imported qualified 
------------------------------------------------------------------------------------------}
{-# LANGUAGE OverloadedStrings #-}
module Smutt.Utility.URLEncoded (urlDecode) where

import Smutt.Utility.Utility

import Data.Monoid 
import Data.Maybe

import Data.Word 

import Smutt.Utility.Hex


import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI



type EscapedChar = B.ByteString






percentEncodedToByte :: B.ByteString -> Maybe Word8
percentEncodedToByte "21" = Just $ BI.c2w '!'
percentEncodedToByte "23" = Just $ BI.c2w '#'
percentEncodedToByte "24" = Just $ BI.c2w '$'
percentEncodedToByte "26" = Just $ BI.c2w '&'  
percentEncodedToByte "27" = Just $ BI.c2w '\''   
percentEncodedToByte "28" = Just $ BI.c2w '('  
percentEncodedToByte "29" = Just $ BI.c2w ')'   
percentEncodedToByte "2A" = Just $ BI.c2w '*' 
percentEncodedToByte "2B" = Just $ BI.c2w '+'   
percentEncodedToByte "2C" = Just $ BI.c2w ','   
percentEncodedToByte "2F" = Just $ BI.c2w '/'   
percentEncodedToByte "3A" = Just $ BI.c2w ':'  
percentEncodedToByte "3B" = Just $ BI.c2w ';'   
percentEncodedToByte "3D" = Just $ BI.c2w '='   
percentEncodedToByte "3F" = Just $ BI.c2w '?'   
percentEncodedToByte "40" = Just $ BI.c2w '@'    
percentEncodedToByte "5B" = Just $ BI.c2w '['   
percentEncodedToByte "5D" = Just $ BI.c2w ']'
percentEncodedToByte bytePair = (sequence $ map hexCharToQuartet $ B.unpack bytePair)  >>= (\ (firstHexChar: secondHexChar:_) ->  
                                      Just $ quartetPairToByte firstHexChar secondHexChar)




percentDecode :: B.ByteString -> Maybe B.ByteString 
percentDecode inString = let percentSplitFragments = B.split (BI.c2w '%') inString -- now we should be able to take two caractar of the start of each list fragment. 
                         in fmap B.concat $ decodePercentFragments percentSplitFragments



decodePercentFragments :: [B.ByteString] -> Maybe [B.ByteString]
decodePercentFragments [] = Just [] 
decodePercentFragments (fragment:nextString) = 
    if B.length fragment >= 2 
        then do 
            let (percentEncoded, rest) = B.splitAt 2 fragment
            realByte <- percentEncodedToByte percentEncoded
            nextStep <- decodePercentFragments nextString
            return $ (B.cons realByte rest : nextStep)
        else Nothing 



repalcePlusWithSpace :: Word8 -> Word8 
repalcePlusWithSpace c = 
    if BI.isSpaceWord8 c
    then (BI.c2w ' ')
    else c  

urlDecode :: B.ByteString -> Maybe B.ByteString 
urlDecode str = percentDecode $ B.map repalcePlusWithSpace str




