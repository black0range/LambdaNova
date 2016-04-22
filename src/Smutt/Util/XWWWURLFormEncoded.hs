{-----------------------------------------------------------------------------------------
Module name: xWWWFormURLEncoded
Made by:     Tomas Möre 2015

Utility library.
------------------------------------------------------------------------------------------}
{-# LANGUAGE OverloadedStrings #-}

module Smutt.Util.XWWWURLFormEncoded (XWWWFormURLSerializable)where


import Data.Monoid
import Data.String
import qualified Data.List as List

import qualified Data.ByteString           as B
import qualified Data.ByteString.Internal  as BI
import qualified Data.ByteString.Lazy      as BL

import Smutt.Util.URLEncoding as URL

import Smutt.Util.Char

-- | Since Text data cannot be reliably urldecoded  this is only defined for byteString
-- Since the url decodeing might fail we this is a Maybe value

class (Monoid a, IsString a) => XWWWFormURLSerializable a where
    decode :: a -> Maybe [(a,a)]
    encode :: [(a,a)] -> a

-- Removed because of URL decoding
{-
instance XWWWFormURLSerializable T.Text where
    xWWWFormURLDecode text =
        let firstSplit    = T.split (=='&') text
            secondSplit s = case T.split (=='=') s of
                                (a:b:_) -> (a,b)
                                _       -> ("","")
        in filter  (not . (==("",""))) (map secondSplit firstSplit)


instance XWWWFormURLSerializable TL.Text where
    xWWWFormURLDecode text =
        let firstSplit    = TL.split (=='&') text
            secondSplit s = case TL.split (=='=') s of
                                (a:b:_) -> (a,b)
                                _       -> ("","")
        in filter  (not . (==("",""))) (map secondSplit firstSplit)
-}

instance XWWWFormURLSerializable B.ByteString where
    decode text = filter  (/=("","")) <$> mapM pairSplitter elementSplitted
      where elementSplitted    = B.split (fromChar '&') text
            pairSplitter s = URL.decode $ B.break (==fromChar '=') s

    encode = B.concat . xWWWFormURLEncodeGeneric

instance XWWWFormURLSerializable BL.ByteString where
    decode text = filter (/=("","")) <$> mapM secondSplit firstSplit
      where firstSplit    = BL.split (BI.c2w '&') text
            secondSplit s = URL.decode $ BL.break (==fromChar '=') s

    encode =  BL.concat . xWWWFormURLEncodeGeneric



xWWWFormURLEncodeGeneric :: (XWWWFormURLSerializable a, URL.Serializable a) => [(a,a)] -> [a]
xWWWFormURLEncodeGeneric  =  List.intersperse ";" . map (uncurry elementMaker . URL.encode)
  where
    elementMaker a b = a <> "=" <> b
