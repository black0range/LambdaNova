{-----------------------------------------------------------------------------------------
Module name: WebSockets
Made by:     Tomas Möre 2015


Usage:  This is a WebSockets module inteded to run over an instance of the standard Server  

IMPORTANT: If you start a websockets session DO NOT attempt to send any kind of HTTP request upon completion 



------------------------------------------------------------------------------------------}
--http://datatracker.ietf.org/doc/rfc6455/?include_text=1
{-# LANGUAGE OverloadedStrings #-}
module WebSockets where 

import qualified HTTP
import qualified Headers as H 
import qualified Data.ByteString as B 
import Data.ByteString.Internal (c2w) 
import qualified Data.ByteString.Lazy as BL 
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T 
import qualified Data.Text.Lazy as TL 

import qualified BufferedSocket as BS 

import Data.Maybe 
import Data.Either 
import Data.Monid
import Data.Bits 

import Util
import BufferedSocket 
import ErrorResponses

import Data.List 
import qualified Data.ByteString.Base64 as B64 
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Binary as BINARY


type PayloadLength  = Word64
type Mask           = Maybe [Word8] -- Should be infinte list 
type Fin            = Bool 
type FrameSize      = Int -- Positive Int either 16 or 64
type Masked         = Bool 
type WebSocketThunk = (Request -> IO Response)

type CloseStatusCode = Word16
-- Websocket magic number, don't blame me!! :'(
guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"


data Response =   TextResponse      !T.Text
                | LazyTextResponse  TL.Text 
                | BinaryResponse    !B.ByteString
                | LazyBinaryResponse BL.ByteString
                | NoResponse 
                | CloseResponse StatusCode B.ByteString
                | QuitWebSockets 

data Request  =   TextResponse       !T.Text
                | LazyBinaryResponse  B.ByteString
                | TextResponse       !T.Text
                | LazyTextResponse    TL.Text
                | CloseRequest StatusCode B.ByteString
                | Ping 
                | Pong 

-- 
data SocketStatus = IORef Bool 

data AuthenticationError = InvalidVersion | InvalidMethod | MissingHeader H.HeaderName | InvalidHeader H.HeaderName 

data AuthenticationData = {
    , webSocketKey      :: ByteString
    , origin            :: Maybe ByteString
    , webSocketVersion  :: Int 
    , webSocketProtocol :: [ByteString]
    , socketExtensions  :: [ByteString]
} 

makeWebsocketExtensionList :: H.Headers -> [ByteString]
makeWebsocketExtensionList [] = []
makeWebsocketExtensionList ((H.SecWebSocketExtensions, hdrValue):xs) =  (hdrValue:makeWebsocketExtensionList xs)
makeWebsocketExtensionList (_:xs) = makeWebsocketExtensionList xs 

makeWebsocketProtoclList :: ByteString -> [ByteString]
makeWebsocketProtoclList = map stripWhitespace . B.split (c2w ',') 

authenticateHandshake :: HTTP.Request -> Either AuthenticationError AuthenticationData  
authenticateHandshake  req = 
    case lookup True conditions of 
        Just a  -> Left a 
        Nothing -> Right authenticationData
    where 
        headers = HTTP.requestHeaders req 

        -- Lookups off diffrent header values and making just variables 
        maybeHost@(Just host)                               = lookup H.Host $ headers
        maybeUpgrade@(Just upgrade)                         = lookup H.Upgrade $ headers
        maybeConnection@(Just connection)                   = lookup H.Host $connection
        maybeOrigin                                         = lookup H.Origin $ headers
        maybeWebSocketKey@(Just webSocketKey)               = lookup H.SecWebSocketKey $ headers
        maybeWebSocketVersion@(Just webSocketVersion)       = lookup H.SecWebSocketVersion $ headers
        maybeWebSocketProtocol@(Just webSocketProtocol)     = lookup H.SecWebSocketProtocol $ headers
        --maybeWebSocketExtensions@(Just webSocketExtensions) = lookup H.SecWebSocketExtensions $ headers
        keyDecoded@(Right keyBytestring)                    = B64.decode webSocketKey
        keyIsValid                                          = and [isRight keyDecoded, (B.length keyBytestring) == 16]

        webSocketVersonInt                                  = byteStringToInteger webSocketVersion

        conditions =    [ ( not $ HTTP.reqIsGET req             , InvalidMethod)
                        , ( not $ HTTP.reqIsHTTP11 req          , InvalidVersion)
                        , ( isNothing maybeHost                 , MissingHeader H.Host)
                        , ( isNothing maybeUpgrade              , MissingHeader H.Upgrade)
                        , ( isNothing maybeConnection           , MissingHeader H.Connection)
                        , ( isNothing maybeWebSocketKey         , MissingHeader H.SecWebSocketKey)
                        , ( isNothing maybeWebSocketVersion     , MissingHeader H.SecWebSocketVersion)
                        , ( quickQIEqual upgrade "websocket"    , InvalidHeader H.Upgrade)
                        , ( quickQIEqual connection "upgrade"   , InvalidHeader H.Connection)
                        , ( not keyIsValid                      , InvalidHeader H.SecWebSocketKey)
                        , (webSocketVersion == "13"             , InvalidHeader H.SecWebSocketVersion)
                        ]
        authenticationData = AuthenticationData { webSocketKey      = webSocketKey
                                                , origin            = maybeOrigin
                                                , webSocketVersion  = webSocketVersonInt
                                                , webSocketProtocol = maybe []  makeWebsocketProtoclList webSocketProtocol
                                                , socketExtensions  = makeWebsocketExtensionList headers 
                                                } 


acceptHandshake :: HTTP.Request -> AuthenticationData -> IO ()
acceptHandshake req authData = 
    BS.send bSocket fullResponseString
    where 
        statusLine          = "HTTP/1.1 101 Switching Protocols\r\n"
        respondKey          = (B64.encode (SHA1.hash ((webSocketKey req) <> guid)))
        respondHeaders      = [(H.Connection, "Upgrade"),(H.Upgrade, "websocket"),(H.SecWebSocketAccept, respondKey)]
        fullResponseString  = statusLine <> (H.headersToString respondHeaders) <> crlf
        bSocket = HTTP.bufSocket req 

withWebSockets :: HTTTP.Request -> WebSocketThunk -> IO HTTP.Response 
withWebSockets req thunk = 
    if isRight eitherAuthentication
        then 
            do 
                let Right authData = eitherAuthentication
                acceptHandshake req  authData

                return HTTP.Manual
        else 
            return HTTP.HeadersResponse 400 [(H.Connection, "close")]

    where 
        eitherAuthentication = authenticateHandshake request
        bSocket = HTTP.bufSocket req 

serve :: BufferedSocket ->  WebSocketThunk -> IO ()
serve bSocket thunk = 
    do 

readMessage :: BufferedSocket -> IO Request
readMessage = 

readFrameHeader :: BufferedSocket -> IO Frame
readFrameHeader bSocket = 
    do 
       frameHeader <- BS.readN bSocket 10 
       let (b1:b2)  = B.unpack $ B.take 2 frameHeader
            fin     = isFin b1
            opCode  = toOpCode b1
            hasMask = isMaked b2 
            eitherPayload = (payloadLength8 b2) 
            Right smallPayload   = eitherPayload
            nExtendedBytes  = if isRight eitherPayload then 0 else let Left a = eitherPayload in a 

            (extendBytes:rest1)    =   B.splitAt nExtendedBytes (B.drop 2 frameHeader) -- 2 from first split 
            extendedPayloadLength = shiftWordTo  (B.unpack extendBytes) shiftWordTo

            realPayload     = if nExtendedBytes == 0 then  smallPayload else extendedPayloadLength

            (maskString:rest2) = B.splitAt (if hasMask then 4 else 0) test
            maskList = case maskString of 
                        "" -> Nothing 
                        _  -> cycle $ B.unpack maskString
        appendExcessData bSocket rest2

        return $ Frame fin opcode masked maskList realPayload

data FrameHeader = FrameHeader Fin OpCode Masked Mask PayloadLength Body
{-

Table From http://datatracker.ietf.org/doc/rfc6455/?include_text=1 describing the bit table of a frame. 
This is used to make the functions under 
 0                   1                   2                   3
      0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     +-+-+-+-+-------+-+-------------+-------------------------------+
     |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
     |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
     |N|V|V|V|       |S|             |   (if payload len==126/127)   |
     | |1|2|3|       |K|             |                               |
     +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
     |     Extended payload length continued, if payload len == 127  |
     + - - - - - - - - - - - - - - - +-------------------------------+
     |                               |Masking-key, if MASK set to 1  |
     +-------------------------------+-------------------------------+
     | Masking-key (continued)       |          Payload Data         |
     +-------------------------------- - - - - - - - - - - - - - - - +
     :                     Payload Data continued ...                :
     + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
     |                     Payload Data continued ...                |
     +---------------------------------------------------------------+
-}
{-
 FIN:  1 bit

      Indicates that this is the final fragment in a message.  The first
      fragment MAY also be the final fragment.
-}

isFin :: Word8 -> Fin 
isFin b = (shift b (-7)) == 1

{-
   Opcode:  4 bits

      Defines the interpretation of the "Payload data".  If an unknown
      opcode is received, the receiving endpoint MUST _Fail the
      WebSocket Connection_.  The following values are defined.

      *  %x0 denotes a continuation frame
      *  %x1 denotes a text frame
      *  %x2 denotes a binary frame
      *  %x3-7 are reserved for further non-control frames
      *  %x8 denotes a connection close
      *  %x9 denotes a ping
      *  %xA denotes a pong
      *  %xB-F are reserved for further control frames
-}
data OpCode =  ConinuationFrame | TextFrame | BinaryFrame | ConnectionClose | Ping | Pong | Reserved | Undefined 

toOpCode :: Word8 -> OpCode
toOpCode 0x0 =  ConinuationFrame
toOpCode 0x1 =  TextFrame
toOpCode 0x2 =  BinaryFrame
toOpCode 0x8 =  ConnectionClose
toOpCode 0x9 =  Ping
toOpCode 0xA =  Pong 
toOpCode n 
    | isUndefined = Undefined
    | isReserved  = Reserved
    where 
        isUndefined = (0x3 >= n && 0x7 <= n)
        isReserved  = n >= 0xB
-- zeroes any FIN or RSV bits 
getOpCode :: Word8 -> OpCode 
getOpCode = toOpCode . (240 .&.) 
{-
 Mask:  1 bit

      Defines whether the "Payload data" is masked.  If set to 1, a
      masking key is present in masking-key, and this is used to unmask
      the "Payload data" as per Section 5.3.  All frames sent from
      client to server have this bit set to 1.
-}
-- Same function as isFin
isMaksed :: Word8 -> Bool 
isMasked = isFin

-- Nulls the leftmost bit. IF the remaing is 126 then read extended payload of 16 bytes or if 127 read extended payload of 
payloadLength8 :: Word8 -> Either FrameSize PayloadLength
payloadLength8 b  
    | n < 126 = Right n 
    | n == 126 = Left smallFrame
    | otherwise = Left bigFrame
    where 
        n           = 128 .&. b  -- nulls the first bit  
        smallFrame  = 2 -- 16 bits 
        bigFrame    = 8 -- 8 bytes




data StatusCode =  NormalClose | GoingAway | ProtocolError | NonAcceptableData | InvalidData | ViolatedPolicy | MessageTooBig | NeedsExtension | UnexpectedCondition | CusomCode Word16

statusCodeToByteString :: StatusCode -> B.ByteString
statusCodeToByteString  NormalClose         = "\ETX\232" -- 1000
statusCodeToByteString  GoingAway           = "\ETX\233" -- 1001
statusCodeToByteString  ProtocolError       = "\ETX\234" -- 1002
statusCodeToByteString  NonAcceptableData   = "\ETX\235" -- 1003
statusCodeToByteString  InvalidData         = "\ETX\239" -- 1007
statusCodeToByteString  ViolatedPolicy      = "\ETX\240" -- 1008
statusCodeToByteString  MessageTooBig       = "\ETX\241" -- 1009
statusCodeToByteString  NeedsExtension      = "\ETX\242" -- 1010
statusCodeToByteString  UnexpectedCondition = "\ETX\243" -- 1011
statusCodeToByteString  CusomCode w16       = BINARY.encode w16
