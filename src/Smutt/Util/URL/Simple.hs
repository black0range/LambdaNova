{-# LANGUAGE TypeFamilies, FlexibleInstances, OverloadedStrings, UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes #-}

module Smutt.Util.URL.Simple where

import Data.Monoid
import Data.String
import Data.Maybe
import qualified Smutt.Util.String as Str

-- | Note that this data format confirms to scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]
-- | When converting Url -> string it might not be a correct url if all the fields aren't there
data URL s = URL{
  scheme         :: s,
  authentication :: s,
  host           :: s,
  port           :: s,
  path           :: s,
  query          :: s,
  fragment       :: s
}

instance (Str.StringData s) => Show (URL s)  where
  show = show . toString



emptyUrl :: (IsString s, Monoid s) => URL s
emptyUrl = URL{
  scheme   = mempty,
  authentication = mempty,
  host     = mempty,
  port     = mempty,
  path     = mempty,
  query    = mempty,
  fragment = mempty
}

class Serializable s where
  fromString :: (IsString s) => s -> Maybe (URL s)
  toString   :: (IsString s) => URL s -> s



-- | Pares a string acording to scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]
-- | Could be concidered an injective function
-- Don't even try to make this more beuatiful... generic characters doesn't work.
-- trust me i've tried
-- url format scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]

instance  (Str.StringData s) => Serializable s where
  -- This terrible terrible function needs remaking
  fromString "" = Nothing
  fromString strIn = if  not correctAuthority
    then Nothing
    else Just $ URL schemeStr authenticationStr hostStr portStr pathStr queryStr fragmentStr
    where

      -- acording to the standard a scheme MUST be present
      (schemeStr, schemeUndecidedRest) = case Str.break (==schemeSeparator) strIn of
        (a,"") -> ("",a)
        tuple ->  Str.drop 1 <$> tuple
      -- If the two first characters after the scheme is "//" there is a host else
      -- the remaing structure is [/]path[?query][#fragment]
      hasAuthorityPart = Str.take 2 schemeUndecidedRest == "//"

      schemeRest = if hasAuthorityPart then Str.drop 2 schemeUndecidedRest else schemeUndecidedRest

      -- If there was an authority drop te two slashes and go on
      -- else drop any first ocoring slashes since this is optional if the host was missing.

      (authorityPart, postAuthorityRest) = if hasAuthorityPart
        then case Str.break (==pathSeparator) schemeRest of
              (_,"") -> case Str.break (==querySeparator) schemeRest of
                          (_,"") -> Str.break (==fragmentSeparator) schemeRest
                          a -> a
              a -> a
        else (mempty, Str.dropWhile (==pathSeparator) schemeUndecidedRest)

      -- if there's no athority this does nothing else try to find the authentication separator
      -- which is optional
      (authenticationStr, authenticationRest) = if hasAuthorityPart
        then case Str.break (==authenticationSeparator) authorityPart of
          (a, "")   -> (mempty, a)
          (a, rest) -> (a, Str.drop 1 rest)
       else emptyTuple

       -- if authority as present the host MUST exist
       -- this can end either by finding a ':' indicating a port
       -- else theres '/' indicating path or nothing indicating that we're done
       -- if the url doesn't have authjority  the host rest is simply schemeRest
      (hostStr, portStr) = if hasAuthorityPart
        then case Str.break (==portSeparator) authenticationRest of
          -- if theres no port separator (:) we split at the path instead  and port is mempty
          (hostPart, "")  -> (hostPart,mempty)
          -- if theres a port separator we get the port.
          hostAndPort -> Str.drop 1 <$> hostAndPort
        -- if theres no authority we pass the
        else emptyTuple

      correctAuthority    = (hasAuthorityPart ||  (Str.null hostStr))

      authrestBeginsWPath = Str.take 1 postAuthorityRest == "/"


      -- the path "must exist"
      (pathStr, pathRest) = if authrestBeginsWPath
        then Str.break (\char -> char == querySeparator || char == fragmentSeparator) (Str.tail postAuthorityRest)
        else (mempty, postAuthorityRest)

      pathRestBeginsWithQuery  = Str.take 1 pathRest == "?"

      (queryStr, fragmentStr) = Str.drop 1 <$> if pathRestBeginsWithQuery
        then Str.break  (==fragmentSeparator) (Str.tail pathRest)
        else (mempty, pathRest)

      emptyTuple = (mempty,mempty) :: Monoid s => (s,s)

      schemeSeparator            =  ':'
      authenticationSeparator    =  '@'
      portSeparator              = schemeSeparator
      pathSeparator              =  '/'
      querySeparator             =  '?'
      fragmentSeparator          =  '#' 

  toString url = schemeStr <> authenticationStr <> hostStr <> portStr <> pathStr <> queryStr <> fragmentStr
    where

        schemeStr = whenNotNull (<> schemeEnd) $ scheme url

        authenticationStr = whenNotNull (<> "@") $ authentication url
        hostStr = host url
        portStr = whenNotNull (":"<>) $ port url

        hasAuthory = not $ Str.null hostStr
        schemeEnd = if hasAuthory then "://" else ":"

        pathStr =  "/" <> path url
        queryStr = whenNotNull ("?"<>) $ query url
        fragmentStr = whenNotNull ("#"<>) $ fragment url

        whenNotNull f s = if Str.null s then "" else f s



transform :: (a -> b) -> URL a -> URL b
transform f url = URL (f $ scheme url) (f $ authentication url) (f $ host url) (f $ port url) (f $ path url) (f $ query url) (f $ fragment url)
