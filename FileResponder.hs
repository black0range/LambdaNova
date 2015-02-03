{-# LANGUAGE OverloadedStrings #-}
module FileResponder where 

import qualified Data.ByteString      as B 
import qualified Data.ByteString.Lazy as BL 
import qualified HTTP
import qualified Headers as H
import System.IO 
import System.IO.Unsafe
import Control.Exception 

fileChunks :: Int -> Int -> [Int]
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

fileSender :: FilePath -> IO HTTP.Response
fileSender fp = 
    do 
        handle      <- openBinaryFile fp ReadMode 
        fileSize    <-  hFileSize handle
        let chunkList     = fileChunks (1024 * 8) $ fromIntegral fileSize
            maybeFileSize =  Just $ fromIntegral fileSize

        fileContent <-   unsafeInterleaveIO $ fmap BL.fromChunks $ fileReader handle chunkList
        return $ HTTP.FullLazyResponse 200 [(H.ContentType, "image/jpg")] (fileContent) maybeFileSize



fileSenderStrict :: FilePath -> IO HTTP.Response
fileSenderStrict fp = 
    do 
        fileData   <- B.readFile fp
        return $ HTTP.FullResponse 200 [(H.ContentType, "Image/JPG")] fileData


