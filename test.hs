{-# LANGUAGE OverloadedStrings #-}
import Prelude 
import Server
import qualified HTTP as HTTP 
import qualified Headers as H
import Control.Monad 
import qualified Data.ByteString      as B 
import qualified Data.ByteString.Lazy as BL 
import qualified Data.Text.Lazy as T 
import qualified Data.Text.Lazy.Encoding as T
import qualified Network.HTTP.Base as HB (urlDecode) 
import Data.Maybe 
import Data.Monoid
import System.IO 
import ErrorResponses 
import FileResponder 
--import  Filesystem.Path.CurrentOS
testResponseHeaders = [(H.ContentType, "text/html")]

testfullResponse = "<html><body><h1>Hello world!</h1><body></body>"


main = serve testImageServer --testHandler
--main = serve (\x -> return $ ChukedResponse    200 testResponseHeaders testChunkedBody)
--main = serve (\x -> return $ FullLazyResponse  200 testResponseHeaders testChunkedBody (Just 46))

--ChukedResponse    !StatusCode !ResponseHeaders [ByteString]
removePlusChar [] = []
removePlusChar ('+':xs) = (' ':removePlusChar xs)
removePlusChar (x:xs)   = (x:removePlusChar xs)

urdDecode =  T.pack . HB.urlDecode . removePlusChar . T.unpack 

xWWWFormURLDecode :: T.Text -> [(T.Text,T.Text)]
xWWWFormURLDecode t =  let firstSplit    = T.split (=='&') t
                           secondSplit s = case T.split (=='=') s of 
                                            (a:b:_) -> (a,b)
                                            _       -> ("","")
                        in filter  (not . (==("",""))) (map secondSplit firstSplit)

testHandler :: HTTP.Request -> IO HTTP.Response
testHandler req = 
    do  bytesBody <- (HTTP.requestBody req)
        let requestBody  = T.decodeUtf8 bytesBody
            responseCode = 200
            headers      = [(H.ContentType, "text/html; charset=utf-8")]
            body         = "<html> <body><form method='POST'> <input name='test' type='text'> </input><button type='submit' value='submit'></form></html></body>"
            datadIn      = xWWWFormURLDecode $ urdDecode requestBody
            asd          = fromMaybe "" $ lookup "test" datadIn 
        return $ HTTP.FullLazyResponse responseCode headers (T.encodeUtf8 (asd <> body )) Nothing



testImageServer :: HTTP.Request -> IO HTTP.Response 
testImageServer req = do
    case HTTP.requestPath req of 
        ("strict":_) -> (fileSenderStrict "platypusLg.jpg") 
        ("lazy":_)   -> (fileSender "platypusLg.jpg") 
        _            -> fileNotFoundError
