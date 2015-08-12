{-# LANGUAGE OverloadedStrings #-}
module Smutt.HTTP.Server.FileResponder (
fileSender
) where 

import qualified Data.ByteString      as B 
import qualified Data.ByteString.Lazy as BL 
import qualified Smutt.HTTP.Server.Response as HTTP
import qualified Smutt.HTTP.Headers as H
import System.IO 
import System.IO.Unsafe
import Control.Exception 


type FileSize = Int 
type ChunkMaxSize = Int 
type MimeType = B.ByteString

fileChunks :: ChunkMaxSize -> FileSize -> [Int]
fileChunks _         0        = []
fileChunks chunkSize fileSize   
    | fileSize > chunkSize   = (chunkSize:next)
    | fileSize <= chunkSize  = (fileSize:[])
    where 
        next = fileChunks chunkSize (fileSize - chunkSize)

fileReader :: Handle -> [Int] -> IO [B.ByteString]
fileReader h  []     = hClose h >> return []  
fileReader h  (r:rs) =
    do 
        fileChunk <- B.hGet h r 
        next      <-  unsafeInterleaveIO $ fileReader h rs 
        return (fileChunk:next)





fileSender :: FilePath -> ChunkMaxSize -> H.Headers -> MimeType -> IO HTTP.Response
fileSender fp chunkMaxSize inHeaders mimeType = 
    do 
        handle      <- openBinaryFile fp ReadMode 
        fileSize    <-  hFileSize handle
        let chunkList     = fileChunks chunkMaxSize $ fromIntegral fileSize
            maybeFileSize =  Just $ fromIntegral fileSize

        fileContent <-   unsafeInterleaveIO $ fmap BL.fromChunks $ fileReader handle chunkList
        return $ HTTP.FullLazyResponse 200 ((H.ContentType, mimeType): inHeaders) (fileContent) maybeFileSize

