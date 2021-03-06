{-# LANGUAGE OverloadedStrings #-}

{-
This URI URL thing is truly confusing...
This is how I'll handle it.


If the URI is a star then the server will handle it itself. (never get evaluated)
So the parsing will return nothing.
If the URI is an abs path we'll cheat and just throw it into the URL object
-}

module Smutt.HTTP.URI (
   URI
  , URL (..)
  , fromString
   
                      ) where
import Smutt.Util.String as Str

import Data.Monoid

import Smutt.Util.URL.Simple (URL)
import qualified Smutt.Util.URL.Simple as SimpleURL
import Data.ByteString.Lazy as BL



type URI = URL BL.ByteString

-- Decided to handle it as follows,
-- If the "URI" is a "*" then the server will use a special handle
-- If the "URI" is an absolute
fromString :: (Str.StringData s) => s -> Maybe (URL s)
fromString "" = Nothing
fromString "*" = Nothing
fromString strIn  =  SimpleURL.fromString strIn
