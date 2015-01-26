{-# LANGUAGE OverloadedStrings #-}
import Server
import HTTP 
import qualified Headers as H
main = serve (\x -> return $ FullResponse 200 [(H.ContentType, "text/html")] "<html><body><h1>Hello world!</h1><body></body>")