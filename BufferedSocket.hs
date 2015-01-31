{-# LANGUAGE OverloadedStrings #-}

module BufferedSocket 
(
  readRaw
, BufferedSocket 
, getLine
, readChunked
, makeBufferedSocket
, MaxLineLength
, readNLazy
  )
where 

import Prelude hiding (getLine)

import Util 
import ServerOptions 

import qualified Data.ByteString           as B
import qualified Data.ByteString.Internal  as BI
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Builder   as BB 
import Data.IORef
import Network.Socket

import System.IO.Unsafe -- I'm so sorry for this

import Foreign.ForeignPtr

import Data.Word 

type MaxLineLength    = Int

type BufferSize       = Int
type BufferDataLength = IORef Int
type ExcessData       = IORef ByteString

type ReadSize         = Int 
-- This buffered socket is read by first checking the bufferData ByteString. If that is not enought buffer a bit more data... etc
type BufferedSocket   = (Socket, (ForeignPtr Word8), BufferSize, ExcessData)

type Read = Int  


unsafeCond :: [(Bool,a)] -> a 
unsafeCond ((True, a):_) = a
unsafeCond (_: xs) = unsafeCond xs 

makeBufferedSocket :: Socket -> BufferSize -> IO BufferedSocket
makeBufferedSocket sock  bufferSize = 
    do
      bufferData       <- newIORef  "" 
      buffer           <- BI.mallocByteString bufferSize
      return (sock, buffer, bufferSize, bufferData)


readExcessData :: BufferedSocket  -> IO ByteString
readExcessData (_,_,_, excessData)  = readIORef excessData

writeExcessData :: BufferedSocket -> ByteString -> IO ()
writeExcessData  (_, _, _,excessData)  = writeIORef excessData 

clearExcessData  :: BufferedSocket -> IO ()
clearExcessData  bSocket = writeExcessData  bSocket  ""


readNExcessData :: BufferedSocket -> Int -> IO (Int, ByteString)
readNExcessData bSocket n =
    do 
        allData <- readExcessData bSocket
        let 
            relevant = B.take n allData
            excess   = B.drop n allData
        writeExcessData bSocket excess
        return (B.length relevant, relevant)

breakByteExcessData :: BufferedSocket -> Word8 -> IO (Maybe ByteString)
breakByteExcessData bSocket char = 
    do 
        allData <-  readExcessData bSocket
        let (relevant, rest) = B.breakByte char  allData
        return $
            case rest of 
                "" -> Nothing 
                _  ->   (writeExcessData bSocket $ B.tail rest) `seq` Just relevant   


breakSubstringExcessData :: BufferedSocket -> ByteString -> IO (Maybe ByteString)
breakSubstringExcessData bSocket str =
    do 
        allData <-  readExcessData bSocket 
        let (relevant, rest) = B.breakSubstring str allData
        return $
            case rest of   
                "" -> Nothing 
                _  -> (writeExcessData bSocket $ B.drop (B.length str) rest) `seq` Just relevant   
-- Doesn't care about allready read data 
fillBuffer :: BufferedSocket -> IO Int
fillBuffer (socket, buffer, bufferSize,_) = 
    fmap fst (withForeignPtr buffer socketReader)
    where
        socketReader ptr = recvBufFrom socket ptr bufferSize

-- reads data from positon 0 
readBuffer :: BufferedSocket -> Int -> ByteString
readBuffer  (_,buffer,_,_) read = BI.fromForeignPtr buffer 0 read

readRaw :: BufferedSocket -> IO ByteString
readRaw bSocket = fillBuffer bSocket >>=  return . readBuffer bSocket
{- 
This puts together a new "buffered Socket". or bSock
A buffered socket is a socket which is acommodated by a Ptr. Because of the need to split headers etc up at certain points but still
safet he rest of the data to be whatever the server requires it to be.

A buffered socket should be called with freeBSocket before disregarded. 
The server thunk function does so automaticly.

-}
readToBlankRow :: BufferedSocket -> IO ()
readToBlankRow bSocket =
    do 
        row <- getLine 2048 bSocket 
        case row of 
            Nothing -> error "Too long line while emptying blanks"
            Just "" -> return ()
            Just _  -> readToBlankRow bSocket


readChunked :: BufferedSocket -> IO [ByteString]
readChunked bSocket =
    do 
        maybeLine  <- getLine 2048 bSocket
        excessData <- readExcessData bSocket
        let 
            Just line           = maybeLine
            (hexStr, _)         = B.breakByte (BI.c2w ';') line -- throwing away any extra chunk data
            maybeHex            = byteStringToHex hexStr
        case maybeLine of 
            Nothing -> error "Chunked line lengnth was too long"
            Just  _ -> case maybeHex of 
                            Nothing  -> error "Invalid hex encoding in chunked message"
                            Just hex -> let exessLength = B.length excessData
                                            exessRemaining = (exessLength - hex)
                                        in unsafeCond   [ (hex == 0            ,   readToBlankRow bSocket >> return [])
                                                        , ( exessLength > hex  ,  (writeExcessData bSocket (B.drop  exessRemaining excessData)) >> 
                                                                                      return [B.take exessRemaining excessData])
                                                        , ( exessLength == hex ,   clearExcessData bSocket >> return [excessData])
                                                        , ( otherwise          , do 
                                                                                    clearExcessData bSocket 
                                                                                    let remainingBytes = (hex - exessLength)
                                                                                    next <- unsafeInterleaveIO $ readChunked bSocket
                                                                                    chunkRest <- unsafeInterleaveIO $  readLazyNReal bSocket remainingBytes
                                                                                    
                                                                                    return $ (excessData : chunkRest) ++ next
                                                                                    )
                                                          ] 

readLazyNReal :: BufferedSocket -> ReadSize -> IO [ByteString]
readLazyNReal _ 0 = return []
readLazyNReal bSocket n =
    do 
        dataIn <-  readRaw bSocket
        let read = B.length dataIn 
        if read <= n
            then 
                do 
                    next <- unsafeInterleaveIO $ readLazyNReal bSocket (n - read)
                    return (dataIn: next)
            else 
                 (writeExcessData bSocket (B.drop n dataIn)) >> return [B.take n dataIn]


readNLazy :: BufferedSocket -> ReadSize -> IO [ByteString]
readNLazy bSocket n =
    do 
        bufferedData <-  readExcessData bSocket 
        let bufferedLength  = B.length bufferedData 

        unsafeCond  [ ( n > bufferedLength  , (writeExcessData bSocket (B.drop n bufferedData)) >> return [B.take n bufferedData] )
                    , ( n == bufferedLength , clearExess >> return  [bufferedData]) 
                    , ( otherwise           , do 
                                                clearExess
                                                next <- unsafeInterleaveIO $ readLazyNReal bSocket bufferedLength
                                                return (bufferedData: next))]
    where
        clearExess = clearExcessData bSocket



readNReal :: BufferedSocket -> ReadSize -> IO [ByteString]
readNReal _ 0 = return []
readNReal bSocket n =
    do 
        dataIn <- readRaw bSocket 
        let read = B.length dataIn  
        if read < n 
          then 
            do 
                next <- readN bSocket (n - read)
                return (dataIn:next)
          else 
            writeExcessData bSocket (B.drop (read - n) dataIn) >>
                return [B.take n dataIn]

readN :: BufferedSocket -> ReadSize -> IO [ByteString]
readN _       0 = return []
readN bSocket n = 
    do 
        bufferedData <- readExcessData bSocket
        let dataLength = B.length bufferedData 
        unsafeCond [ ( dataLength > n  , (writeExcessData bSocket (B.drop n bufferedData)) >> (return [B.take n bufferedData]))
                   , ( dataLength == n , clearExess >> (return [bufferedData]))
                   , ( otherwise       , clearExess >> (fmap (bufferedData:) $ readNReal bSocket (n - dataLength) ))]
    where
        clearExess = clearExcessData bSocket

-- # Following are two very similary looking functions. The first one allwasy cheacks if the pre read data firs then if that fails it attempts to read more # --

-- This function gets a line from the BufferedSocket.
-- However this is required to accept lines that are a maximum length of the buffer size
-- Probably should change this in the future somehow
getLineReal :: BufferedSocket -> ByteString -> MaxLineLength -> IO (Maybe ByteString)
getLineReal bSocket buffered  maxLength = 
    if reachedMaxSize
        then
            do 
                writeExcessData bSocket buffered
                return Nothing 
        else 
            do
                string <-  readRaw bSocket
                let crlfSplit    = B.breakSubstring crlf string
                    stringLength = B.length string
                case crlfSplit of
                            (_,"")               -> getLineReal bSocket (B.append buffered string) maxLength
                            (returnString, rest) -> do   
                                                        writeExcessData bSocket (B.drop crlfLength rest)
                                                        return $ Just returnString
    where 
        reachedMaxSize = (B.length buffered) >= maxLength

-- Starts the "Real" function.
-- This is also responsible for rebuffering the buffer if necssesarry
getLine :: MaxLineLength -> BufferedSocket -> IO (Maybe ByteString)  
getLine maxLength bSocket  =
    do  
        bufferData    <- readExcessData bSocket

        let crlfSplit = B.breakSubstring crlf bufferData
        
        case crlfSplit of 
            (_,"")           -> getLineReal bSocket bufferData maxLength
            (relevent, rest) -> if stringToBig relevent 
                                    then 
                                        return Nothing 
                                    else 
                                        do 
                                            writeExcessData bSocket (B.drop crlfLength rest)
                                            return $ Just relevent 
    where 
        stringToBig = (maxLength <=) . B.length 












