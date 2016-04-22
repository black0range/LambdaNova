{-# LANGUAGE OverloadedStrings #-}
module Smutt.HTTP.Header where



import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HS
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Monoid

import Prelude hiding (lookup)

newtype Header = Header (HashMap Text Text) deriving (Eq)

toHashMap :: Header -> HashMap Text Text
toHashMap (Header hdr) = hdr


-- | Warning this function is a bit slow and should generally not be used
-- 
fromHashMap :: HashMap Text Text -> Header
fromHashMap  = fromList . map (\(k,v) -> (T.toCaseFold k, v)) . HS.toList

toString :: Header -> Text
toString (Header hdr)= HS.foldrWithKey (\ k v  s -> s <> k <> ": " <> v <> "\r\n") "" hdr <> "\r\n"

instance Show Header where
  show = show . toString

instance Monoid Header where
  mempty = Header mempty
  mappend (Header a) (Header b) = (Header (mappend a b))
  mconcat [] = mempty
  mconcat hdrs = Header $ mconcat $ map (\ (Header a) -> a) hdrs 



null :: Header -> Bool
null = (==mempty)

size :: Header -> Int
size = HS.size . toHashMap

member :: Text -> Header -> Bool
member key = HS.member (T.toCaseFold key) . toHashMap 

lookup :: Text -> Header -> Maybe Text
lookup key =  HS.lookup (T.toCaseFold key) . toHashMap

-- | Checks if there are a header k with value v
fieldIn :: Text -> Text -> Header -> Bool
fieldIn k v hdr = case lookup (T.toCaseFold k)  hdr of
  Just a -> a == v
  _ -> False 

insert :: Text -> Text -> Header -> Header
insert key val = Header . HS.insert (T.toCaseFold key) val . toHashMap

toList :: Header -> [(Text,Text)]
toList (Header hdr) = HS.toList hdr


fromList :: [(Text,Text)] -> Header
fromList = Header . HS.fromList . map (\(k,v) -> (T.toCaseFold k, v))


keys :: Header -> [Text]
keys  = HS.keys . toHashMap
