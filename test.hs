{-# LANGUAGE OverloadedStrings #-}
import Server
import HTTP 
import qualified Headers as H

testResponseHeaders = [(H.ContentType, "text/html")]

testfullResponse = "<html><body><h1>Hello world!</h1><body></body>"
testChunkedBody  = ["<html><body><h", "1>Hello world!", "</h1><body></body>"]
testSimpleHello = "Hello World!"
main = serve (\x -> return $ FullResponse 200 [(H.ContentType, "text/plain")] testSimpleHello)
--main = serve (\x -> return $ ChukedResponse    200 testResponseHeaders testChunkedBody)
--main = serve (\x -> return $ FullLazyResponse  200 testResponseHeaders testChunkedBody (Just 46))

--ChukedResponse    !StatusCode !ResponseHeaders [ByteString]