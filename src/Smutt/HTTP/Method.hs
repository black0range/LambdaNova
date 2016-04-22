{-----------------------------------------------------------------------------------------
Module name: Smutt.HTTP.Method
Made by:     Tomas Möre 2016

Meant to be imported qualified
------------------------------------------------------------------------------------------}
{-# LANGUAGE OverloadedStrings #-}
module Smutt.HTTP.Method (fromString, Method(..), methodHashMap, toString) where

import qualified Data.HashMap.Strict as HM
import Data.String  hiding (fromString)


import Smutt.Util.String as Str

data Method = OPTIONS | GET | HEAD | POST | PUT | DELETE | TRACE | CONNECT
  deriving(Show, Eq)

fromString :: (StringData s) => s -> Maybe Method
fromString = (`HM.lookup` methodHashMap)

toString :: (StringData s) => Method -> s
toString OPTIONS = "OPTIONS"
toString GET = "GET"
toString HEAD = "HEAD"
toString POST = "POST"
toString PUT = "PUT"
toString DELETE = "DELETE"
toString TRACE = "TRACE"
toString CONNECT = "CONNECT"


methodList :: [Method]
methodList = [OPTIONS, GET, HEAD, POST, PUT, DELETE, TRACE, CONNECT]

methodStringList :: (IsString s) => [s]
methodStringList = ["OPTIONS", "GET", "HEAD", "POST", "PUT", "DELETE", "TRACE", "CONNECT"]

methodHashMap :: (StringData s) => HM.HashMap s Method
{-# NOINLINE methodHashMap #-}
methodHashMap = HM.fromList $ zip  methodStringList methodList
