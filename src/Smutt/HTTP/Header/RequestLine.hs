{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Smutt.HTTP.Header.RequestLine where


import Smutt.HTTP.Method as Method
import Smutt.HTTP.Version as Version

import Smutt.Util.URL.Simple as URL
import Smutt.HTTP.URI as URI
import qualified Smutt.Util.String as Str

import Data.Monoid

import Smutt.HTTP.Error

data RequestLine s = RequestLine{
  method  :: Method,
  uriRaw  :: s,
  uri     :: URL s,
  version ::Version
} deriving (Show)


fromString :: (Str.StringData s, VersionString s) => s -> Either ReaderError (RequestLine s)
fromString "" = Left $ BadRequest "Empty request-line"
fromString str = if length splitted /= requiredWords
  then Left $ BadRequest "Not enougth words in request-line"
  else do
    method <- withErrorMsg "Method not recognized."  $ Method.fromString methodStr
    uri    <- withErrorMsg "Invalid URI" $ URI.fromString uriStr
    version <- withErrorMsg "Invalid HTTP version string" $ Version.fromString  versionStr
    pure $ RequestLine method uriStr uri version

  where
    splitted = Str.words str
    [methodStr, uriStr, versionStr] = splitted
    requiredWords = 3
    withErrorMsg e  = maybe (Left $ BadRequest e) Right 


toString :: (Str.StringData s, VersionString s) => RequestLine s -> s
toString (RequestLine method uriRaw uri version) =  Method.toString method <> " " <> uriRaw <> " " <> Version.toString version
