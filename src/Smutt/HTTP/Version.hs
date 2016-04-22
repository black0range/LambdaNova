{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module Smutt.HTTP.Version where

import Data.Monoid
import Data.String hiding (fromString)

import Data.ByteString as B
import Data.ByteString.Builder as B
import Data.ByteString.Char8 as B
import Data.ByteString.Lazy as BL

import Data.Text as T
import Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder  as TLB
import Data.Text.Lazy.Builder.Int as TLB
import Data.Text.Read as T

import qualified Smutt.Util.String as Str

import Data.Char (isDigit)

import Data.Either

type Major = Int
type Minor = Int

data Version = HTTP {major ::Major, minor ::Minor} deriving (Show)

instance Eq Version where
  (==) a b = major a == major b && minor a == minor b

instance Ord Version where
  compare a b = case compare (major a) (major b) of
    EQ -> compare (minor a) (minor b)
    ordering -> ordering

  (<) a b = compare a b == LT

  (<=) a b = LT == coparision || coparision == EQ
    where coparision = compare a b

  (>) a b = compare a b  == GT

  (>=) a b = GT == comparison || comparison == EQ
    where comparison = compare a b

  max a b = if a > b then a else b

  min a b = if a < b then a else b


-- Needed since theres no standard class for strings to integers
class VersionString s where
  fromString :: s -> Maybe Version
  toString   :: Version -> s


instance VersionString B.ByteString where
  fromString s = parse s  >>=
    (\ (majorStr,minorStr) -> do  majorInt <- fst <$> B.readInt majorStr
                                  minorInt <- fst <$> B.readInt minorStr
                                  Just (HTTP majorInt minorInt))
  toString (HTTP majorInt minorInt) = genericToString (strMaker majorInt) (strMaker minorInt)
    where
      strMaker = BL.toStrict . B.toLazyByteString . B.intDec

instance VersionString BL.ByteString where
  fromString = fromString . BL.toStrict
  toString   = BL.fromStrict . toString

instance VersionString T.Text where
  fromString s = parse s >>= (\ (majorStr,minorStr) ->
   let eitherMajorResult = T.decimal majorStr
       eitherMinorReulst = T.decimal minorStr
       Right (majorInt,_) = eitherMajorResult
       Right (minorInt,_) =eitherMinorReulst
   in if isRight eitherMajorResult && isRight eitherMinorReulst
     then Just $ HTTP majorInt minorInt
     else Nothing)

  toString = TL.toStrict . toString

instance VersionString TL.Text where
  fromString = fromString . TL.toStrict
  toString (HTTP majorInt minorInt) = genericToString majorStr minorStr
    where
      majorStr = TLB.toLazyText $ TLB.decimal majorInt
      minorStr =  TLB.toLazyText $ TLB.decimal minorInt


genericToString :: (Str.StringData s) => s -> s -> s
genericToString majorStr minorStr = "HTTP/" <> majorStr <> "." <> minorStr

parse :: (Str.StringData s) => s -> Maybe (s,s)
parse ""  = Nothing
parse strIn
  | protocol /= "HTTP" = Nothing
  | validVersions && (numberStringCorrect majorStr && numberStringCorrect minorStr) = Just (majorStr,minorStr)
  | otherwise = Nothing
  where
    (protocol,rest) =  Str.drop 1 <$> Str.break (=='/') strIn
    (majorStr, minorStr) = Str.drop 1 <$> Str.break (== '.') rest
    validVersions = not (Str.null majorStr) && not (Str.null minorStr)
    numberStringCorrect "" = False
    numberStringCorrect s = not (Str.head s == '0'  && Str.length s > 1)  && Str.all isDigit s
