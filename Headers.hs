{-----------------------------------------------------------------------------------------
Module name: Headers
Made by:     Tomas Möre 2015

This module is mean to be imported qualified 

ex: import Headers qualified H

------------------------------------------------------------------------------------------}


{-# LANGUAGE OverloadedStrings #-}
module Headers where


import Util
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Data.Time.Clock

type Header           = (HeaderName, ByteString)
type Headers          = [Header]




data HeaderName =     AIM                       
                  |   Accept                    
                  |   AcceptAdditions          
                  |   AcceptCharset            
                  |   AcceptEncoding           
                  |   AcceptFeatures           
                  |   AcceptLanguage           
                  |   AcceptRanges             
                  |   Age                       
                  |   Allow                     
                  |   Alternates                
                  |   AuthenticationInfo       
                  |   Authorization             
                  |   CExt                     
                  |   CMan                     
                  |   COpt                     
                  |   CPEP                     
                  |   CPEPInfo                
                  |   CacheControl             
                  |   Connection                
                  |   ContentBase              
                  |   ContentDisposition       
                  |   ContentEncoding          
                  |   ContentID                
                  |   ContentLanguage          
                  |   ContentLength            
                  |   ContentLocation          
                  |   ContentMD5               
                  |   ContentRange             
                  |   ContentScriptType       
                  |   ContentStyleType        
                  |   ContentType              
                  |   ContentVersion           
                  |   Cookie                    
                  |   Cookie2                   
                  |   DAV                       
                  |   Date                      
                  |   DefaultStyle             
                  |   DeltaBase                
                  |   Depth                     
                  |   DerivedFrom              
                  |   Destination               
                  |   DifferentialID           
                  |   Digest                    
                  |   ETag                      
                  |   Expect                    
                  |   Expires                   
                  |   Ext                       
                  |   From                      
                  |   GetProfile                
                  |   Host                      
                  |   IM                        
                  |   If                        
                  |   IfMatch                  
                  |   IfModifiedSince         
                  |   IfNoneMatch             
                  |   IfRange                  
                  |   IfUnmodifiedSince       
                  |   KeepAlive                
                  |   Label                     
                  |   LastModified             
                  |   Link                      
                  |   Location                  
                  |   LockToken                
                  |   MIMEVersion              
                  |   Man                       
                  |   MaxForwards              
                  |   Meter                     
                  |   Negotiate                 
                  |   Opt                       
                  |   OrderingType             
                  |   Overwrite                 
                  |   P3P                       
                  |   PEP                       
                  |   PICSLabel                
                  |   PepInfo                  
                  |   Position                  
                  |   Pragma                    
                  |   ProfileObject             
                  |   Protocol                  
                  |   ProtocolInfo             
                  |   ProtocolQuery            
                  |   ProtocolRequest          
                  |   ProxyAuthenticate        
                  |   ProxyAuthenticationInfo 
                  |   ProxyAuthorization       
                  |   ProxyFeatures            
                  |   ProxyInstruction         
                  |   Public                    
                  |   Range                     
                  |   Referer                   
                  |   RetryAfter
                  |   TransferEncoding
                  |   Custom ByteString
                  deriving (Show, Eq, Read)

dateHeader :: IO Header
dateHeader =  makeHTTPDate >>= return . (\n -> (Date , n)) 
               

chunkedHeader :: Header
chunkedHeader = (TransferEncoding, "chunked")


nameToString :: HeaderName -> ByteString
nameToString AIM                      = "AIM"                  
nameToString Accept                   = "Accept"              
nameToString AcceptAdditions          = "Accept-Additions"      
nameToString AcceptCharset            = "Accept-Charset"     
nameToString AcceptEncoding           = "Accept-Encoding"   
nameToString AcceptFeatures           = "Accept-Features"
nameToString AcceptLanguage           = "Accept-Language"
nameToString AcceptRanges             = "Accept-Ranges"
nameToString Age                      = "Age"
nameToString Allow                    = "Allow"
nameToString Alternates               = "Alternates"
nameToString AuthenticationInfo       = "AuthenticationInfo"
nameToString Authorization            = "Authorization"
nameToString CExt                     = "C-Ext"
nameToString CMan                     = "C-Man"
nameToString COpt                     = "C-Opt"
nameToString CPEP                     = "C-PEP"
nameToString CPEPInfo                 = "C-PEP-Info"
nameToString CacheControl             = "Cache-Control"
nameToString Connection               = "Connection"
nameToString ContentBase              = "Content-Base"
nameToString ContentDisposition       = "Content-Disposition"
nameToString ContentEncoding          = "Content-Encoding"
nameToString ContentID                = "Content-ID"
nameToString ContentLanguage          = "Content-Language"
nameToString ContentLength            = "Content-Length"
nameToString ContentLocation          = "Content-Location"
nameToString ContentMD5               = "Content-MD5"
nameToString ContentRange             = "Content-Range"
nameToString ContentScriptType        = "Content-Script-Type"
nameToString ContentStyleType         = "Content-Style-Type"
nameToString ContentType              = "Content-Type"
nameToString ContentVersion           = "Content-Version"
nameToString Cookie                   = "Cookie" 
nameToString Cookie2                  = "Cookie2"
nameToString DAV                      = "DAV"
nameToString Date                     = "Date"
nameToString DefaultStyle             = "Default-Style"
nameToString DeltaBase                = "Delta-Base"
nameToString Depth                    = "Depth"
nameToString DerivedFrom              = "Derived-From"
nameToString Destination              = "Destination"
nameToString DifferentialID           = "Differential-ID"
nameToString Digest                   = "Digest"
nameToString ETag                     = "E-Tag"
nameToString Expect                   = "Expect"
nameToString Expires                  = "Expires"
nameToString Ext                      = "Ext"
nameToString From                     = "From"
nameToString GetProfile               = "Get-Profile" 
nameToString Host                     = "Host"
nameToString IM                       = "IM"
nameToString If                       = "If"
nameToString IfMatch                  = "If-Match"
nameToString IfModifiedSince          = "If-Modified-Since"
nameToString IfNoneMatch              = "If-None-Match"
nameToString IfRange                  = "If-Range"
nameToString IfUnmodifiedSince        = "If-Unmodified-Since"
nameToString KeepAlive                = "Keep-Alive"
nameToString Label                    = "Label"
nameToString LastModified             = "Last-Modified"
nameToString Link                     = "Link"
nameToString Location                 = "Location"
nameToString LockToken                = "Lock-Token"
nameToString MIMEVersion              = "MIME-Version"
nameToString Man                      = "Man"
nameToString MaxForwards              = "Max-Forwards"
nameToString Meter                    = "Meter"
nameToString Negotiate                = "Negotiate"
nameToString Opt                      = "Opt"
nameToString OrderingType             = "Ordering-Type"
nameToString Overwrite                = "Overwrite"
nameToString P3P                      = "P3P"
nameToString PEP                      = "PEP"
nameToString PICSLabel                = "PICS-Label"
nameToString PepInfo                  = "Pep-Info"
nameToString Position                 = "Position"
nameToString Pragma                   = "Pragma"
nameToString ProfileObject            = "Profile-Object"
nameToString Protocol                 = "Protocol"
nameToString ProtocolInfo             = "Protocol-Info"
nameToString ProtocolQuery            = "Protocol-Query"
nameToString ProtocolRequest          = "Protocol-Request"
nameToString ProxyAuthenticate        = "Proxy-Authenticate"
nameToString ProxyAuthenticationInfo  = "Proxy-AuthenticationInfo"
nameToString ProxyAuthorization       = "Proxy-Authorization"
nameToString ProxyFeatures            = "Proxy-Features"
nameToString ProxyInstruction         = "Proxy-Instruction"
nameToString Public                   = "Public"      
nameToString Range                    = "Range"          
nameToString Referer                  = "Referer"           
nameToString RetryAfter               = "Retry-After"
nameToString TransferEncoding         = "Transfer-Encoding"
nameToString (Custom str)             = str

stringToName :: ByteString -> HeaderName
stringToName "AIM"                        = AIM              
stringToName "Accept"                     = Accept        
stringToName "Accept-Additions"           = AcceptAdditions
stringToName "Accept-Charset"             = AcceptCharset    
stringToName "Accept-Encoding"            = AcceptEncoding 
stringToName "Accept-Features"            = AcceptFeatures
stringToName "Accept-Language"            = AcceptLanguage
stringToName "Accept-Ranges"              = AcceptRanges
stringToName "Age"                        = Age
stringToName "Allow"                      = Allow
stringToName "Alternates"                 = Alternates
stringToName "AuthenticationInfo"         = AuthenticationInfo
stringToName "Authorization"              = Authorization
stringToName "C-Ext"                      = CExt
stringToName "C-Man"                      = CMan
stringToName "C-Opt"                      = COpt
stringToName "C-PEP"                      = CPEP
stringToName "C-PEP-Info"                 = CPEPInfo
stringToName "Cache-Control"              = CacheControl
stringToName "Connection"                 = Connection
stringToName "Content-Base"               = ContentBase
stringToName "Content-Disposition"        = ContentDisposition
stringToName "Content-Encoding"           = ContentEncoding
stringToName "Content-ID"                 = ContentID
stringToName "Content-Language"           = ContentLanguage
stringToName "Content-Length"             = ContentLength
stringToName "Content-Location"           = ContentMD5
stringToName "Content-MD5"                = ContentMD5
stringToName "Content-Range"              = ContentRange
stringToName "Content-Script-Type"        = ContentScriptType
stringToName "Content-Style-Type"         = ContentStyleType
stringToName "Content-Type"               = ContentType
stringToName "Content-Version"            = ContentVersion
stringToName "Cookie"                     = Cookie
stringToName "Cookie2"                    = Cookie2
stringToName "DAV"                        = DAV
stringToName "Date"                       = Date
stringToName "Default-Style"              = DefaultStyle           
stringToName "Delta-Base"                 = DeltaBase
stringToName "Depth"                      = Depth
stringToName "Derived-From"               = DerivedFrom
stringToName "Destination"                = Destination
stringToName "Differential-ID"            = DifferentialID
stringToName "Digest"                     = Digest           
stringToName "E-Tag"                      = ETag
stringToName "Expect"                     = Expect
stringToName "Expires"                    = Expires
stringToName "Ext"                        = Ext
stringToName "From"                       = From
stringToName "Get-Profile"                = GetProfile
stringToName "Host"                       = Host
stringToName "IM"                         = IM
stringToName "If"                         = If
stringToName "If-Match"                   = IfMatch
stringToName "If-Modified-Since"          = IfModifiedSince
stringToName "If-None-Match"              = IfNoneMatch
stringToName "If-Range"                   = IfRange
stringToName "If-Unmodified-Since"        = IfUnmodifiedSince
stringToName "Keep-Alive"                 = KeepAlive
stringToName "Label"                      = Label
stringToName "Last-Modified"              = LastModified
stringToName "Link"                       = Link
stringToName "Location"                   = Location
stringToName "Lock-Token"                 = LockToken
stringToName "MIME-Version"               = MIMEVersion
stringToName "Man"                        = Man
stringToName "Max-Forwards"               = MaxForwards
stringToName "Meter"                      = Meter
stringToName "Negotiate"                  = Negotiate
stringToName "Opt"                        = Opt
stringToName "Ordering-Type"              = OrderingType
stringToName "Overwrite"                  = Overwrite
stringToName "P3P"                        = P3P
stringToName "PEP"                        = PEP
stringToName "PICS-Label"                 = PICSLabel
stringToName "Pep-Info"                   = PepInfo
stringToName "Position"                   = Position
stringToName "Pragma"                     = Pragma
stringToName "Profile-Object"             = ProfileObject
stringToName "Protocol"                   = Protocol
stringToName "Protocol-Info"              = ProtocolInfo
stringToName "Protocol-Query"             = ProtocolQuery
stringToName "Protocol-Request"           = ProtocolRequest
stringToName "Proxy-Authenticate"         = ProxyAuthenticate
stringToName "Proxy-AuthenticationInfo"   = ProxyAuthenticationInfo
stringToName "Proxy-Authorization"        = ProxyAuthorization
stringToName "Proxy-Features"             = ProxyFeatures
stringToName "Proxy-Instruction"          = ProxyInstruction
stringToName "Public"                     = Public
stringToName "Range"                      = Range
stringToName "Referer"                    = Referer
stringToName "Retry-After"                = RetryAfter
stringToName "Transfer-Encoding"          = TransferEncoding
stringToName  str                         = Custom str

toString:: Header -> ByteString
toString (a, b) = B.concat [(nameToString a), " : ", b]

headersToString:: Headers -> ByteString
headersToString []     = ""
headersToString (x:xs) = B.append (toString x) (headersToString xs)

toSendRow:: Header -> ByteString
toSendRow s = B.append (toString s) crlf 
