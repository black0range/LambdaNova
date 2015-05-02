{-----------------------------------------------------------------------------------------
Module name: BufferedSocketHTTPEXT
Made by:     Tomas Möre 2014


Usage:  Defines extetions to BufferedSocket that are specificly made to read from the http protocol



------------------------------------------------------------------------------------------}

{-# LANGUAGE OverloadedStrings #-}
module BufferedSocketHTTPEXT  where  

import Prelude hiding (getLine)



import qualified Data.ByteString      as B
import qualified Data.ByteString.Char8 as B (words) 
import qualified Data.ByteString.Internal  as BI
import qualified Data.ByteString.Lazy  as BL

import Data.Functor 
import Util

import BufferedSocket 

import System.IO.Error
import System.IO.Unsafe -- I'm so sorry for this


readToBlankRow :: BufferedSocket -> IO ()
readToBlankRow bSocket =
    do 
        row <- getLine 2048 bSocket 
        case row of 
            Nothing -> error "Too long line while emptying blanks"
            Just "" -> return ()
            Just _  -> readToBlankRow bSocket


readChunkedReal :: BufferedSocket -> IO [ByteString]
readChunkedReal bSocket =
    do 
        maybeLine  <- getLine 2048 bSocket
        let 
            Just line           = maybeLine
            (hexStr, _)         = B.breakByte (BI.c2w ';') line -- throwing away any extra chunk data
            maybeHex            = byteStringToHex hexStr
        case maybeLine of 
            Nothing -> error "Chunked line lengnth was too long"
            Just  _ -> case maybeHex of 
                            Nothing  -> error "Invalid hex encoding in chunked message"
                            Just hex -> if hex == 0 
                                        then readToBlankRow bSocket >> return []
                                        else do 
                                                chunk <- unsafeInterleaveIO $ readRaw bSocket hex
                                                next <- unsafeInterleaveIO $ readChunkedReal bSocket
                                                return (chunk:next)
readChunked:: BufferedSocket -> IO BL.ByteString
readChunked = (BL.fromChunks <$>). readChunkedReal


getLine :: MaxLength -> BufferedSocket -> IO (Maybe ByteString)  
getLine maxLength bSocket  = readToByteStringMax bSocket crlf maxLength 
