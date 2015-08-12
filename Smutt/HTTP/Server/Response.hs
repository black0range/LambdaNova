{-----------------------------------------------------------------------------------------
Module name: Resposnse
Made by:     Tomas Möre 2015

It is reccomended to import this module qualified as HTTP

------------------------------------------------------------------------------------------}

{-# LANGUAGE OverloadedStrings #-}
module Smutt.HTTP.Server.Response where

import Smutt.HTTP.Common


import qualified Smutt.HTTP.Headers as H
import Smutt.Utility.Utility



import qualified Data.ByteString      as B
import qualified Data.ByteString.Char8 as B (words) 
import qualified Data.ByteString.Internal  as BI
import qualified Data.ByteString.Lazy  as BL
--import Network.HTTP.Types


import Data.Maybe

type ResponseHeaders = Headers


{-
-------------
FullResponse
-------------
A Full Response sends a full ByteString as a response.
The server will count the length of the ByteString and send the apporpiate content length.

If keep alive is set in server options and |Connection: Close| is not part of the header fields.
The server will keep listening on the connection

-----------------
FullLazyResponse
-----------------
Same as as full response but takes a lazy list.
This requires the |ContentLength| field to be set.
This enables space effitient sending of data


If |ContentLength| is set to |Nothing| the server will calculate the length of the entire list before sending. 
This method however might be verry performance space unefficient

WARNING! If ContentLength IS set and there are errors in the chunks there server Will NOT be able to send an error message


-----------------
ChukedResponse
-----------------
Chunked response accepts a list of Strict ByteStrings 

If there 
-}

data Response =   FullResponse      !StatusCode !ResponseHeaders !ByteString
                | FullLazyResponse  !StatusCode !ResponseHeaders BL.ByteString !(Maybe ContentLength)
                | ChunkedResponse   !StatusCode !ResponseHeaders BL.ByteString
                | HeadersResponse   !StatusCode !ResponseHeaders
                | ManualResponse 
                | NoResponse         






