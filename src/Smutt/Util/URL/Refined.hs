{-
This is a refi
-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, OverloadedStrings, UndecidableInstances, ScopedTypeVariables,
             AllowAmbiguousTypes #-}

module Smutt.Util.URL.Refined where


import Data.Monoid
import Data.String
import Data.Maybe

import qualified Smutt.Util.URL.Simple as Simple
import Smutt.Util.URL.Query
import Smutt.Util.URL.Path
import qualified Smutt.Util.String as Str

import qualified Smutt.Util.URLEncoding as URL

-- | Note that this data format confirms to scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]
-- | When converting Url -> string it might not be a correct url if all the fields aren't there
data URL s = URL{
  scheme         :: s,
  username       :: s,
  password       :: s,
  host           :: s,
  port           :: s,
  path           :: [s],
  query          :: [(s,s)],
  fragment       :: s
} deriving (Show)



emptyUrl :: (IsString s, Monoid s) => URL s
emptyUrl = URL{
  scheme   = mempty,
  username = mempty,
  password = mempty,
  host     = mempty,
  port     = mempty,
  path     = [],
  query    = [],
  fragment = mempty
}

class Serializable s where
  fromString :: (IsString s) => s -> Maybe (URL s)
  toString   :: (IsString s) => URL s -> s

instance (Str.StringData s, PathString s, QueryString s) => Serializable s where
  fromString strIn = if isNothing maybeSimpleUrl
    then Nothing
    else Just $ URL (Simple.scheme simpleURL) usernameStr passwordStr (Simple.host simpleURL)
                    (Simple.port simpleURL) pathList queryMap (Simple.fragment simpleURL)
    where
      maybeSimpleUrl = Simple.fromString strIn
      simpleURL      = fromJust maybeSimpleUrl
      (usernameStr,passwordStr) = Str.drop 1 <$> Str.break (==userpwdSeparator) (Simple.authentication simpleURL)
      pathStr  = Simple.path simpleURL
      pathList = toPath pathStr
      queryMap = toQuery (Simple.query simpleURL)

      userpwdSeparator   = ':'

  toString = Simple.toString . fromSimpleURL


fromSimpleURL :: (Str.StringData s, PathString s, QueryString s) => URL s -> Simple.URL s
fromSimpleURL refinedURL = Simple.URL (scheme refinedURL) authentication (host refinedURL) (port refinedURL) pathStr queryStr (fragment refinedURL)
  where
    authentication = if Str.null (password refinedURL) then username refinedURL <> ":" <> password refinedURL else username refinedURL
    pathStr        = fromPath (path refinedURL)
    queryStr       = fromQuery (query refinedURL)


-- | applies a function to each element of an URL (Sorry i do not know a better word for this)
apply :: (a -> b) -> URL a -> URL b
apply f url = URL mappedScheme mappedUsername mappedPassword mappedHost mappedPort mappedPath mappedQuery mappedFragment
      where
        mappedScheme   = f $ scheme url
        mappedUsername = f $ username url
        mappedPassword = f $ password url
        mappedHost     = f $ host url
        mappedPort    = f $ port url
        mappedPath     = map f $ path url
        mappedQuery    = map (\ (a,b) -> (f a, f b)) $ query url
        mappedFragment = f $ fragment url

decode :: (URL.Serializable s) => URL s -> Maybe (URL s)
decode url = do
  decodedScheme <- URL.decode $ scheme url
  decodedUsername <- URL.decode $ username url
  decodedPassword <- URL.decode $ password url
  decodedmappedHost <- URL.decode $ host url
  decodedPath <- mapM URL.decode $ path url
  decodedQuery <- mapM (\ (k, v) -> do dk <- URL.decode k; dv <- URL.decode v; Just (dk,dv)) $ query url
  Just $ URL decodedScheme decodedUsername decodedPassword decodedmappedHost (port url) decodedPath decodedQuery (fragment url)

encode :: (URL.Serializable s) => URL s -> URL s
encode url = url{
  scheme = URL.encode $ scheme url,
  username  = URL.encode $ username url,
  password  = URL.encode $ password url,
  host  = URL.encode $ host url,
  path  = map URL.encode $ path url,
  query = map (\ (k,v) -> (URL.encode k, URL.encode v)) $ query url
}
