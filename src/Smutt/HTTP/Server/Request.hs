{-# LANGUAGE BangPatterns, OverloadedStrings#-}
module Smutt.HTTP.Server.Request  where

import Prelude hiding (read)

import Smutt.HTTP.Method as Method
import Smutt.Util.URL.Simple as URL
import Smutt.HTTP.URI as URI
import Smutt.HTTP.Version as Version


import Smutt.HTTP.Header (Header)
import qualified Smutt.HTTP.Header as Header 
import Smutt.HTTP.Server.Settings as EXPORT

import Smutt.HTTP.Server.Request.Content.Error



import Network.BufferedSocket (BufferedSocket)

import Control.Concurrent.MVar 

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.ByteString.Lazy (ByteString)

import Data.Maybe
import Data.Monoid



-- | The server applies a Request to the handler thunk for every requst
data Request = Request{
    method          :: !Method
  , uriStr          :: !ByteString
  , uri             :: URI
  , version         :: !Version
  , headers         :: !Header
                         
  , contentLength   :: !(Maybe Int)
  , bodyType        :: !(Maybe BodyType)
                      
  , bufferedSocket  :: !BufferedSocket
  , settings        :: !ServerSettings

  , readStateMVar   :: !(Maybe (MVar ReadState))
    -- | Thest functions are for reading the request. May not allways be the same function depending on the request 
  , readAll ::  IO (Either ContentError  ByteString)
  , readMax :: Int -> IO (Either ContentError (Int, ByteString))
}

-- | Only shows the request header
instance Show Request where
  show req = show (method req) <> " " <> " " <> show (uriStr req) <> " " <> show (version req)  <> show (headers req)

blankRequest :: ServerSettings -> BufferedSocket -> Request
blankRequest set bSock = Request {
  method         = GET,
  uriStr         = "",
  uri            = fromJust $ URI.fromString "/", 
  version        = HTTP 1 0,
  headers        = mempty,
  contentLength  = Nothing,
  bodyType       = Nothing,
  settings       = set,
  bufferedSocket = bSock,
  readStateMVar = Nothing,
  readAll = return $ Left EOF,
  readMax  = \_ -> return $ Left EOF
  
                  }


data BodyType = Chunked | Exact deriving (Eq, Show,Read)

{-|
The read state is in use when a request has a body that should be read.
-}
data ReadState = ExactState {
  exactBytesLeft  :: Int
  } | ChunkedState {
  chunkedTotalRead :: Int,
  chunkBytesLeft   :: Int,
  chunkedEOF       :: Bool,
  trailer          :: Maybe Header
                     
  } deriving (Show,Eq)





-- | Return the ammount of bytes read. If the request doesn't have a body it will still return 0 
bytesRead:: Request -> IO Int
bytesRead req = case readStateMVar req of
  Nothing -> return 0
  Just mvarReadState -> do
    readState <- readMVar mvarReadState
    return $ readStateBytesRead  readState
    
  where
    readStateBytesRead :: ReadState -> Int
    readStateBytesRead (ExactState n) = n
    readStateBytesRead (ChunkedState n _ _ _) = n 

  

-- | Checks wether the body is transfered through chunked encoding 
hasChunkedBody :: Request -> Bool
hasChunkedBody =  maybe False (==Chunked) . bodyType


-- | Checks wether the body length is known 
hasExactBody :: Request -> Bool
hasExactBody = isJust . contentLength

hasBody :: Request -> Bool
hasBody = isJust . bodyType


reachedEOF :: Request -> IO Bool
reachedEOF req 
  | isNothing maybeRS || isNothing mLength  = return True
  | otherwise =  do
      rs <- readMVar (fromJust maybeRS)
      case rs of
        ExactState 0 -> return True
        (ChunkedState _ _ True _) -> return True
        _ -> return False
  where
    maybeRS = readStateMVar req
    mLength = contentLength req




-- | Wrapper for a hashmap lookup, this is needed since field names Must be case-insesitive
headerLookup :: Request -> Text -> Maybe Text
headerLookup req fieldName = Header.lookup fieldName (headers req) 

hasHeaderKey :: Request -> Text -> Bool
hasHeaderKey req key = Header.member key (headers req)

headerSize :: Request -> Int
headerSize = Header.size . headers



hasConnectionClose :: Request -> Bool
hasConnectionClose req =
  case headerLookup req "connection" >>= Just . T.toCaseFold of
    Just "close" -> True
    _ -> False 

