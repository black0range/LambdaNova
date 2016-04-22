{-----------------------------------------------------------------------------------------
Module name: URLEncoding.Serializable
Made by:     Tomas Möre 2015

Simply serves as a class for diffrent serializable objects
------------------------------------------------------------------------------------------}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Smutt.Util.URLEncoding.Serializable where

import Smutt.Util.URLEncoding.ByteString


import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- | Defined for ByteString since Text needs an encoding we leave that to the end user
class Serializable s where
  encode :: s -> s
  decode :: s -> Maybe s



instance Serializable B.ByteString where
  encode = urlEncodeByteString
  decode = urlDecodeByteString


instance Serializable BL.ByteString where
  encode = BL.fromChunks . map encode . BL.toChunks
  decode str = BL.fromChunks <$> mapM decode (BL.toChunks str)


instance (Serializable s) => Serializable (s,s) where
  encode (a,b) = (encode a,encode b)

  decode (a,b) = do
    decodedA <- decode a
    decodedB <- decode b
    pure (decodedA, decodedB)
