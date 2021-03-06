
module Smutt.Util.String where


import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Data.Char as Char

import Data.String

import Data.Hashable

import qualified Prelude


import Data.Eq
import Data.Monoid
import Prelude (Show, Bool, Int, fromIntegral, (.), (<$>), (+), (-), (*), (^), Integral)
import Data.Maybe

--import Data.List  as List
-- | This class is made for very generic string functions and may not be the best alternative for all cases
-- I reccomend to use a specific sting type in applicaitons.
class (IsString s, Monoid s, Eq s, Show s, Hashable s, Prelude.Ord s) => StringData s where
  empty :: s
  singleton :: Char-> s
  pack   :: [Char] -> s
  unpack :: s -> [Char]

  cons   :: Char-> s -> s
  snoc   :: s -> Char-> s
  append :: s -> s -> s
  head   :: s -> Char
  uncons :: s -> Maybe (Char, s)
  last   :: s -> Char
  tail   :: s -> s
  init   :: s -> s
  null   :: s -> Bool
  length :: s -> Int    -- Compromize

  map :: (Char-> Char) -> s -> s
  reverse :: s -> s
  intersperse ::  Char-> s -> s
  intercalacte :: s -> [s] -> s
  transpose :: [s] -> [s]

  concat :: [s] -> s
  concatMap :: (Char -> s) -> s -> s
  any :: (Char-> Bool) -> s -> Bool
  all :: (Char-> Bool) -> s-> Bool
  maximum :: s -> Char
  minimum :: s -> Char

  take :: Int -> s -> s
  drop :: Int -> s -> s
  splitAt :: Int -> s -> (s,s)
  takeWhile :: (Char-> Bool) -> s -> s
  dropWhile :: (Char-> Bool) -> s -> s
  span :: (Char-> Bool) -> s -> (s, s)
  -- skipping spanEnd

  break  :: (Char-> Bool) -> s -> (s,s)

  split :: Char-> s -> [s]
  splitwith :: (Char-> Bool) -> s -> [s]

  lines :: s -> [s]
  words :: s -> [s]
  unlines :: [s] -> s
  unwords :: [s] -> s

  elem :: Char-> s -> Bool
  notElem :: Char-> s -> Bool

  find :: (Char-> Bool) -> s -> Maybe Char
  filter :: (Char-> Bool) -> s -> s

  index :: s -> Int -> Char

  findIndex :: Char-> s -> Maybe Int

  lowercase :: s -> s
  uppercase :: s -> s





-- | A generic string class. Doesn't handle functions for specific data related cases
instance StringData B.ByteString where
  empty = B.empty
  singleton = B.singleton
  pack   = B.pack
  unpack = B.unpack

  cons   = B.cons
  snoc   = B.snoc
  append = B.append
  head   = B.head
  uncons = B.uncons
  last   = B.last
  tail   = B.tail
  init   = B.init
  null   = B.null
  length = B.length

  map = B.map
  reverse = B.reverse
  intersperse = B.intersperse
  intercalacte = B.intercalate
  transpose = B.transpose

  concat = B.concat
  concatMap = B.concatMap
  any = B.any
  all = B.all
  maximum = B.maximum
  minimum = B.minimum

  take = B.take
  drop = B.drop
  splitAt = B.splitAt
  takeWhile = B.takeWhile
  dropWhile = B.dropWhile
  span = B.span
  -- skipping spanEnd

  break = B.break

  split = B.split
  splitwith = B.splitWith

  lines = B.lines
  words = B.words
  unlines = B.unlines
  unwords = B.unwords


  elem = B.elem
  notElem = B.notElem

  find = B.find
  filter = B.filter

  index =  B.index

  findIndex = B.elemIndex


  lowercase = B.map Char.toLower
  uppercase = B.map Char.toUpper


              

instance StringData BL.ByteString where
  empty = BL.empty
  singleton = BL.singleton
  pack   = BL.pack
  unpack = BL.unpack

  cons   = BL.cons
  snoc   = BL.snoc
  append = BL.append
  head   = BL.head
  uncons = BL.uncons
  last   = BL.last
  tail   = BL.tail
  init   = BL.init
  null   = BL.null
  length = fromIntegral . BL.length

  map = BL.map
  reverse = BL.reverse
  intersperse = BL.intersperse
  intercalacte = BL.intercalate
  transpose = BL.transpose

  concat = BL.concat
  concatMap = BL.concatMap
  any = BL.any
  all = BL.all
  maximum = BL.maximum
  minimum = BL.minimum

  take = BL.take . fromIntegral
  drop = BL.drop . fromIntegral
  splitAt = BL.splitAt . fromIntegral
  takeWhile = BL.takeWhile
  dropWhile = BL.dropWhile
  span = BL.span
  -- skipping spanEnd

  break = BL.break

  split = BL.split
  splitwith = BL.splitWith

  lines = BL.lines
  words = BL.words
  unlines = BL.unlines
  unwords = BL.unwords

  elem = BL.elem
  notElem = BL.notElem

  find = BL.find
  filter = BL.filter

  index s = BL.index s . fromIntegral

  findIndex c s = fromIntegral <$> BL.elemIndex c s

  lowercase = BL.map Char.toLower
  uppercase = BL.map Char.toUpper



instance StringData T.Text where
  empty = T.empty
  singleton = T.singleton
  pack   = T.pack
  unpack = T.unpack

  cons   = T.cons
  snoc   = T.snoc
  append = T.append
  head   = T.head
  uncons = T.uncons
  last   = T.last
  tail   = T.tail
  init   = T.init
  null   = T.null
  length = T.length

  map = T.map
  reverse = T.reverse
  intersperse = T.intersperse
  intercalacte = T.intercalate
  transpose = T.transpose

  concat = T.concat
  concatMap = T.concatMap
  any = T.any
  all = T.all
  maximum = T.maximum
  minimum = T.minimum

  take = T.take
  drop = T.drop
  splitAt = T.splitAt
  takeWhile = T.takeWhile
  dropWhile = T.dropWhile
  span = T.span
  -- skipping spanEnd

  break = T.break

  split = T.split .(==)
  splitwith = T.split

  lines = T.lines
  words = T.words
  unlines =T.unlines
  unwords = T.unwords


  find = T.find
  filter = T.filter

  index =  T.index

  findIndex =  T.findIndex . (==)

  lowercase = T.map Char.toLower
  uppercase = T.map Char.toUpper


instance StringData TL.Text where
  empty = TL.empty
  singleton = TL.singleton
  pack   = TL.pack
  unpack = TL.unpack

  cons   = TL.cons
  snoc   = TL.snoc
  append = TL.append
  head   = TL.head
  uncons = TL.uncons
  last   = TL.last
  tail   = TL.tail
  init   = TL.init
  null   = TL.null
  length = fromIntegral . TL.length

  map = TL.map
  reverse = TL.reverse
  intersperse = TL.intersperse
  intercalacte = TL.intercalate
  transpose = TL.transpose

  concat = TL.concat
  concatMap = TL.concatMap
  any = TL.any
  all = TL.all
  maximum = TL.maximum
  minimum = TL.minimum

  take = TL.take . fromIntegral
  drop = TL.drop . fromIntegral
  splitAt = TL.splitAt . fromIntegral
  takeWhile = TL.takeWhile
  dropWhile = TL.dropWhile
  span = TL.span
  -- skipping spanEnd

  break = TL.break

  split = TL.split . (==)
  splitwith = TL.split

  lines = TL.lines
  words = TL.words
  unlines = TL.unlines
  unwords = TL.unwords

  find = TL.find
  filter = TL.filter

  index s =  TL.index s . fromIntegral

  findIndex  chr s = find (TL.toChunks s) 0
    where
      find [] _ = Nothing
      find (str:rest) prev = let found = T.findIndex (==chr) str
                        in case found of
                          Nothing -> find rest (prev + T.length str)
                          Just i  -> Just (i + prev)





readIntegral :: (StringData s, Integral n) => s -> Maybe (n,s)
readIntegral str = if null digits then Nothing else Just (resultInt, rest)
  where
    signed = head str == '-'
    digits = takeWhile Char.isDigit (if signed then tail str else str)
    intCharLength = length digits + if signed then 1 else 0
    intValue = sumDigits digits (length digits - 1) 0
    resultInt = if signed then intValue * (-1) else intValue
    rest = drop intCharLength str


-- | use readInt instead
sumDigits :: (StringData s,Integral n) => s -> Int -> Int -> n
sumDigits _ (-1) _ = 0
sumDigits str i exponent = fromIntegral (Char.digitToInt (index str i)) * (10 ^ exponent) + sumDigits str (i - 1) (exponent + 1)
