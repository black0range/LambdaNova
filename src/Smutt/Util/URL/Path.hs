{-# LANGUAGE OverloadedStrings #-}

module Smutt.Util.URL.Path (PathString, toPath, fromPath) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Smutt.Util.Char


class PathString p where
  toPath   :: p -> [p]
  fromPath :: [p] -> p



instance PathString B.ByteString where
  toPath = dropWhile (==mempty) . B.split (fromChar '/')
  fromPath = B.intercalate "/"

instance PathString BL.ByteString where
  toPath =  dropWhile (==mempty) . BL.split (fromChar '/')
  fromPath = BL.intercalate "/"

instance PathString T.Text where
  toPath =  dropWhile (==mempty) .  T.split (=='/')
  fromPath = T.intercalate "/"

instance PathString TL.Text where
  toPath =  dropWhile (==mempty) . TL.split (=='/')
  fromPath = TL.intercalate "/"
