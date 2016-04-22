{-----------------------------------------------------------------------------------------
Module name: Smutt.HTTP.Common
Made by:     Tomas Möre 2015

This module contains what is important to the HTTP protocol as  whole

------------------------------------------------------------------------------------------}

{-# LANGUAGE OverloadedStrings #-}

module Smutt.HTTP.Common where

import Smutt.Utility.Utility
import Smutt.Utility.URLEncoded

import qualified Network.BufferedSocket
import qualified Smutt.HTTP.Headers as H


import qualified Data.ByteString as B
import qualified Data.ByteString.Internal  as BI

import qualified Network.BufferedSocket as BS


type PathString       = ByteString

type PathFragment     = ByteString
type PathFragments    = [PathFragment]


type ContentLength = Integer

type StatusCode    = Int

type Query   = [(ByteString, ByteString)]
type QueryRaw = ByteString

type Header  = H.Header
type Headers = H.Headers

type Host = ByteString


-- As of now these are all the supported HTTP formats. If new important fomrats shows up they will be added here
data Version = HTTP09 | HTTP10 | HTTP11
    deriving (Eq, Ord)
-- show defenitnions of the HTTP version
instance Show Version where
    show HTTP09 = "HTTP/0.9"
    show HTTP10 = "HTTP/1.0"
    show HTTP11 = "HTTP/1.1"

isHTTP11 = (==HTTP11)
isHTTP10 = (==HTTP10)
isHTTP09 = (==HTTP09)



-- All diffrent used HTTP methods. Common web servers mostly uses POST and GET. However all other methods are supported as well.
-- It is up to the user of this library to decide what happends in all cases.

data Method = OPTIONS | GET | HEAD | POST | PUT | DELETE | TRACE | CONNECT
    deriving(Show, Eq)

-- Just a set of predefined check functions for ease of use
isOPTIONS = (==OPTIONS)
isPOST    = (==POST)
isGET     = (==GET)
isHEAD    = (==HEAD)
isPUT     = (==PUT)
isDELETE  = (==DELETE)
isTRACE   = (==TRACE)
isCONNECT = (==CONNECT)


splitPath :: PathString -> PathFragments
splitPath pathStr = filter (/=B.empty) (B.split (BI.c2w '/') pathStr)

parseQuery :: ByteString -> Maybe Query
parseQuery  queryRaw =
    let atSplit     = B.split (BI.c2w '&') queryRaw
        keyValSplit =  map (B.breakByte (BI.c2w '=')) atSplit
    in  urlDecodeQuery keyValSplit

urlDecodeQuery :: Query -> Maybe Query
urlDecodeQuery [] = Just []
urlDecodeQuery (("",_):rest) = urlDecodeQuery rest
urlDecodeQuery ((keyRaw,valRaw):rest) = do
    key <- urlDecode keyRaw
    val <- urlDecode valRaw
    next <- urlDecodeQuery rest
    return ((key,val) : next)




parseVersion :: ByteString -> Maybe Version
parseVersion "HTTP/0.9" = Just HTTP09
parseVersion "HTTP/1.0" = Just HTTP10
parseVersion "HTTP/1.1" = Just HTTP11
parseVersion _          = Nothing

versionToString  :: Version -> ByteString
versionToString HTTP09 = "HTTP/0.9"
versionToString HTTP10 = "HTTP/1.0"
versionToString HTTP11 = "HTTP/1.1"

parseMethod :: ByteString -> Maybe Method
parseMethod "OPTIONS"     = Just OPTIONS
parseMethod "GET"         = Just GET
parseMethod "HEAD"        = Just HEAD
parseMethod "POST"        = Just POST
parseMethod "PUT"         = Just PUT
parseMethod "DELETE"      = Just DELETE
parseMethod "TRACE"       = Just TRACE
parseMethod "CONNECT"     = Just CONNECT
parseMethod _ = Nothing

-- constant for empty Header
emptyHeader :: Header
emptyHeader = (H.stringToName B.empty, B.empty)


isEmptyHeader :: Header -> Bool
isEmptyHeader a = a == emptyHeader

headerSplitter :: ByteString -> Header
headerSplitter a =  let (name, valueRaw) = B.breakByte (BI.c2w  ':') a
                        value = stripWhitespace $ B.tail valueRaw
                    in (H.stringToName name, value)

parseHeaders :: [ByteString] -> Headers
parseHeaders inData  = [splitted | raw <- inData, let splitted = headerSplitter raw,  not $ isEmptyHeader splitted ]
