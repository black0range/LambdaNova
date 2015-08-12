{-----------------------------------------------------------------------------------------
Module name: xWWWFormURLEncoded
Made by:     Tomas Möre 2015

Utility library. 
------------------------------------------------------------------------------------------}
{-# LANGUAGE OverloadedStrings #-}

module Smutt.Utility.XWWWFormURLEnceded (
        xWWWFormURLDecode
    ,   xWWWFormURLEncode

    )where


import Data.Monoid 
import Data.String 

import qualified Data.ByteString           as B
import qualified Data.ByteString.Internal  as BI
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Builder   as BB 

import qualified Data.Text.Lazy as TL 
import qualified Data.Text as T 


class (Monoid a, IsString a) => XWWWFormURLSerializable a where 
    xWWWFormURLDecode :: a       -> [(a,a)]

joinHeadNTail :: (XWWWFormURLSerializable a) => (a,a) -> a
joinHeadNTail (head, tail) = head <> "=" <> tail



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

instance XWWWFormURLSerializable B.ByteString where 
    xWWWFormURLDecode text = 
        let firstSplit    = B.split (BI.c2w '&') text
            secondSplit s = case B.split (BI.c2w '=') s of 
                                (a:b:_) -> (a,b)
                                _       -> ("","")
        in filter  (not . (==("",""))) (map secondSplit firstSplit)

instance XWWWFormURLSerializable BL.ByteString where 
    xWWWFormURLDecode text = 
        let firstSplit    = BL.split (BI.c2w '&') text
            secondSplit s = case BL.split (BI.c2w '=') s of 
                                (a:b:_) -> (a,b)
                                _       -> ("","")
        in filter  (not . (==("",""))) (map secondSplit firstSplit)


xWWWFormURLEncode :: (XWWWFormURLSerializable a ) => [(a,a)] -> a 
xWWWFormURLEncode  (first:[]) = joinHeadNTail first
xWWWFormURLEncode (first:rest) =  joinHeadNTail first <> mconcat (map ( (";"<>). joinHeadNTail) rest)









