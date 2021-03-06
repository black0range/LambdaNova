{-# LANGUAGE OverloadedStrings #-}
module Smutt.Util.URLEncoding.ByteString (urlEncodeByteString, urlDecodeByteString) where


import Data.Monoid
import Data.Word

import Smutt.Util.Char
import Smutt.Util.URLEncoding.Util

import qualified Smutt.Util.Hex.String as Hex

import qualified Data.ByteString as B

import qualified Smutt.Util.String as Str
import Blaze.ByteString.Builder as BB



-- Decoing from here
urlDecodeByteString :: B.ByteString -> Maybe B.ByteString
urlDecodeByteString str = urlDecode' mempty str 0


urlDecode' :: BB.Builder -> B.ByteString -> Int -> Maybe B.ByteString
urlDecode' builder "" _ = Just $ BB.toByteString builder
urlDecode' builder str currentIndex
  | currentIndex >= B.length str =  Just $ BB.toByteString (builder <> BB.fromByteString str)
  | currentChar == fromChar '%' = if B.length hexStr == 2
    then case maybeHexDecoded of
            Nothing -> Nothing
            Just hexDecoded -> urlDecode' (builder <> purePart <> BB.fromByteString hexDecoded) rest 0
    else Nothing
  | currentChar == fromChar '+' = urlDecode' (builder <> purePart <> BB.fromByteString " ") (B.tail str) 0
  | otherwise = urlDecode' builder str (currentIndex + 1)
  where
    currentChar = B.index str currentIndex
    purePart    = BB.fromByteString $ B.take currentIndex str
    hexRest     = B.drop (currentIndex + 1) str
    hexStr      = B.take 2 hexRest
    maybeHexDecoded  = B.singleton <$> (Hex.toIntegral hexStr :: Maybe Word8)
    rest        = B.drop 2 hexRest





-- Encoding

urlEncodeByteString :: B.ByteString -> B.ByteString
urlEncodeByteString "" = ""
urlEncodeByteString str = urlEncoder' mempty str 0


urlEncoder' :: Builder -> B.ByteString -> Int -> B.ByteString
urlEncoder' builder str currentIndex
  | B.null str =   BB.toByteString builder
  | currentIndex < B.length str = case maybeReplaceWith of
        Nothing -> urlEncoder' builder str (currentIndex + 1)
        Just replaceStr -> let purePart     = B.take currentIndex str
                               remainingStr = B.drop (currentIndex + 1) str
                               newBuider = builder <> BB.fromByteString purePart <>  BB.fromByteString replaceStr
                           in urlEncoder' newBuider remainingStr 0
 | otherwise =   BB.toByteString (builder <> BB.fromByteString (B.take currentIndex str))
 where
    maybeReplaceWith = encodedWord8 $ B.index str currentIndex
