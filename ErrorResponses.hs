{-----------------------------------------------------------------------------------------
Module name: ErrorResponses
Made by:     Tomas Möre 2015
------------------------------------------------------------------------------------------}
{-# LANGUAGE OverloadedStrings #-}
module ErrorResponses where 

import Data.ByteString
import HTTP
import qualified Headers as H 

internalServerError :: IO Response
internalServerError = return $ FullResponse 200 [(H.ContentType, "text/html")] "<html><body><h1>404 - Internal Server Error</h1></body></html>"



