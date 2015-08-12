{-----------------------------------------------------------------------------------------
Module name: ErrorResponses
Made by:     Tomas Möre 2015
------------------------------------------------------------------------------------------}
{-# LANGUAGE OverloadedStrings #-}
module Smutt.HTTP.Server.ErrorResponses where 

import Data.ByteString
import Smutt.HTTP.Server.Response
import qualified Smutt.HTTP.Headers as H 

internalServerError :: IO Response
internalServerError = return $ FullResponse 400 [(H.ContentType, "text/html")] "<html><body><h1>400 - Internal Server Error</h1></body></html>"

fileNotFoundError :: IO Response
fileNotFoundError = return $ FullResponse 404 [(H.ContentType, "text/html")] "<html><body><h1>404 - File not found</h1></body></html>" 

badRequestError :: IO Response 
badRequestError = return $ FullResponse 400 [(H.ContentType, "text/html")] "<html><body><h1>400 - Bad request</h1></body></html>" 