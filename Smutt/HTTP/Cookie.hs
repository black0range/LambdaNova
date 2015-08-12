{-----------------------------------------------------------------------------------------
Module name: Cookie
Made by:     Tomas Möre 2015
------------------------------------------------------------------------------------------}

{-# LANGUAGE OverloadedStrings #-}
module Smutt.HTTP.Cookie where

import qualified Data.ByteString           as B
import qualified Data.ByteString.Internal  as BI

import Smutt.Utility.Utility

type Cookie           = (ByteString, ByteString)
type Cookies          = [Cookie]




cookieInnerSplit :: ByteString -> Cookie
cookieInnerSplit a = let (nameRaw, valueRaw) =  B.breakByte (BI.c2w '=') a
                         name                =  stripWhitespace nameRaw
                         value               =  stripWhitespace $ B.drop 1 valueRaw
                     in (name, value)

parseCookies :: ByteString -> Cookies
parseCookies cookieString = 
    let cookieListRaw   = B.split (BI.c2w ';') cookieString
    in map cookieInnerSplit cookieListRaw