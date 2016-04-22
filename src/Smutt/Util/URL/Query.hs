{-| Library to parse any kind of url path |-}
{-# LANGUAGE OverloadedStrings #-}

module Smutt.Util.URL.Query (QueryString, toQuery, fromQuery) where

import Data.String
import Data.Monoid


import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Smutt.Util.Char

import Data.List

class QueryString s where
  toQuery   :: s -> [(s,s)]
  fromQuery :: [(s,s)] -> s


instance QueryString B.ByteString where
  toQuery = map ((B.drop 1 <$>) . B.break isEqualsSign) . B.split (fromChar '&')
  fromQuery = B.concat . addSigns

instance QueryString BL.ByteString where
  toQuery = map ((BL.drop 1 <$>) . BL.break isEqualsSign) . BL.split (fromChar '&')
  fromQuery = BL.concat . addSigns

instance QueryString T.Text where
  toQuery = map ((T.drop 1 <$>) . T.break (=='=')) . T.split (=='&')
  fromQuery = T.concat . addSigns

instance QueryString TL.Text where
  toQuery = map ((TL.drop 1 <$>) . TL.break (=='=')) . TL.split (=='&')
  fromQuery = TL.concat . addSigns

isEqualsSign :: IsChar c => c -> Bool
isEqualsSign  = (== fromChar '=')
{-# INLINE isEqualsSign #-}


addSigns :: (Monoid s, IsString s) => [(s,s)] -> [s]
addSigns  = intersperse "&" .  map (uncurry (\ a b -> a <> "=" <> b))
