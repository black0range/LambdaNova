{-----------------------------------------------------------------------------------------
Module name: Header.Type
Made by:     Tomas Möre 2015
------------------------------------------------------------------------------------------}

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Smutt.HTTP.Header.Reader (read) where

import Prelude hiding (read)

import qualified Smutt.Util.String as Str
import qualified Smutt.HTTP.Header.Field as Field
import Smutt.HTTP.Header as Header

import Smutt.Util.ByteString

import Smutt.HTTP.Error
import qualified Network.BufferedSocket as BS
import Network.BufferedSocket (BufferedSocket)


import Data.Text.Lazy (Text)

import Control.Monad.Except

--  Reader needs MaxLineLength, MaxHeaderSize and HeaderBytesRead
type MaxLength = Int
type MaxFieldLength = MaxLength
type MaxHeaderLength = MaxLength


--- NOTE TO SELF THIS NEEDS CLEANUP

-- | reads the header part of a HTTP request directly from the socket.

read ::  MaxFieldLength -> MaxHeaderLength ->  BufferedSocket -> ExceptT ReaderError IO Header 
read  maxFieldLength maxHeaderLength bSocket  = do
  headerReader <- lift $ bufferedSocketReader maxFieldLength maxHeaderLength bSocket
  ExceptT $ return $ readHeaderLoop mempty headerReader
  where
    -- Mr reader (probably just myself) sorry 
    readHeaderLoop :: Header -> Reader -> Either ReaderError Header
    readHeaderLoop header reader = case reader of
      (Reader (Left readerError) _) ->  Left readerError
      (Reader (Right (Just (key,val))) (Just next)) -> readHeaderLoop (Header.insert key val header) next
      (Reader (Right Nothing) _) -> Right header
      _ -> error "Done goofed" -- Remove this future self





-- | Reader object for simplified Reading 
data Reader = Reader (Either ReaderError  (Maybe (Text, Text))) (Maybe Reader)
  


-- | Creates a Reader
bufferedSocketReader :: MaxFieldLength -> MaxHeaderLength -> BufferedSocket -> IO Reader
bufferedSocketReader maxLineLength maxHeaderLength bSocket = do
  eitherReadResult <- runExceptT $ readHeaderLine maxLineLength maxHeaderLength bSocket
  case eitherReadResult of
    Left readerError -> return $ Reader (Left readerError) Nothing -- Any reading error
    Right result -> case result of
      Nothing -> return $ Reader (Right Nothing) Nothing -- End of header reached
      Just (bytesRead, field) -> do
        nextReader <- bufferedSocketReader maxLineLength (maxHeaderLength - bytesRead) bSocket -- Everything went fine
        return $ Reader (Right (Just field)) (Just nextReader)


-- | Reads a line from the socket in the correct manner
readHeaderLine :: MaxFieldLength -> MaxHeaderLength -> BufferedSocket -> ExceptT ReaderError IO (Maybe (Int, (Text, Text)))
readHeaderLine maxLineLenth maxHeaderLength bSocket = do
  lineString <- ExceptT $ maybe (Left HeaderTooLarge) Right <$> BS.readToSequenceLimited bSocket crlf readLimit
  if Str.null lineString
  then return Nothing
  else case Field.parse lineString of
      Just field -> return $ Just (Str.length lineString, field)
      Nothing -> throwError $ BadRequest ("Not properly fromated header string " ++ (show lineString))

  where
    readLimit = if maxHeaderLength < maxLineLenth then maxHeaderLength else maxLineLenth
