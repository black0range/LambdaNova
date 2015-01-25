{-----------------------------------------------------------------------------------------
This module is mean to be imported qualified 

ex: import Headers qualified H

------------------------------------------------------------------------------------------}


{-# LANGUAGE OverloadedStrings #-}
module Headers where
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
                  |   Custom Bytestring
               
headerNameToString :: (IsString a) => HeaderName -> a
headerNameToString AIM                      = "AIM"                  
headerNameToString Accept                   = "Accept"              
headerNameToString AcceptAdditions          = "Accept-Additions"      
headerNameToString AcceptCharset            = "Accept-Charset"     
headerNameToString AcceptEncoding           = "Accept-Encoding"   
headerNameToString AcceptFeatures           = "Accept-Features"
headerNameToString AcceptLanguage           = "Accept-Language"
headerNameToString AcceptRanges             = "Accept-Ranges"
headerNameToString Age                      = "Age"
headerNameToString Allow                    = "Allow
headerNameToString Alternates               = "Alternates
headerNameToString AuthenticationInfo       = "AuthenticationInfo
headerNameToString Authorization            = "Authorization
headerNameToString CExt                     = "C-Ext"
headerNameToString CMan                     = "C-Man"
headerNameToString COpt                     = "C-Opt"
headerNameToString CPEP                     = "C-PEP"
headerNameToString CPEPInfo                 = "C-PEP-Info"
headerNameToString CacheControl             = "Cache-Control"
headerNameToString Connection               = "Connection"
headerNameToString ContentBase              = "Content-Base"
headerNameToString ContentDisposition       = "Content-Disposition"
headerNameToString ContentEncoding          = "Content-Encoding"
headerNameToString ContentID                = "Content-ID"
headerNameToString ContentLanguage          = "Content-Language"
headerNameToString ContentLength            = "Content-Length"
headerNameToString ContentLocation          = "Content-Location"
headerNameToString ContentMD5               = "Content-MD5"
headerNameToString ContentRange             = "Content-Range"
headerNameToString ContentScriptType        = "Content-Script-Type"
headerNameToString ContentStyleType         = "Content-Style-Type"
headerNameToString ContentType              = "Content-Type"
headerNameToString ContentVersion           = "Content-Version"
headerNameToString Cookie                   = "Cookie" 
headerNameToString Cookie2                  = "Cookie2"
headerNameToString DAV                      = "DAV"
headerNameToString Date                     = "Date"
headerNameToString DefaultStyle             = "Default-Style"
headerNameToString DeltaBase                = "Delta-Base"
headerNameToString Depth                    = "Depth"
headerNameToString DerivedFrom              = "Derived-From"
headerNameToString Destination              = "Destination"
headerNameToString DifferentialID           = "Differential-ID"
headerNameToString Digest                   = "Digest"
headerNameToString ETag                     = "E-Tag"
headerNameToString Expect                   = "Expect"
headerNameToString Expires                  = "Expires"
headerNameToString Ext                      = "Ext"
headerNameToString From                     = "From"
headerNameToString GetProfile               = "Get-Profile" 
headerNameToString Host                     = "Host"
headerNameToString IM                       = "IM"
headerNameToString If                       = "If"
headerNameToString IfMatch                  = "If-Match"
headerNameToString IfModifiedSince          = "If-Modified-Since"
headerNameToString IfNoneMatch              = "If-None-Match"
headerNameToString IfRange                  = "If-Range"
headerNameToString IfUnmodifiedSince        = "If-Unmodified-Since"
headerNameToString KeepAlive                = "Keep-Alive"
headerNameToString Label                    = "Label"
headerNameToString LastModified             = "Last-Modified"
headerNameToString Link                     = "Link"
headerNameToString Location                 = "Location"
headerNameToString LockToken                = "Lock-Token"
headerNameToString MIMEVersion              = "MIME-Version"
headerNameToString Man                      = "Man"
headerNameToString MaxForwards              = "Max-Forwards"
headerNameToString Meter                    = "Meter"
headerNameToString Negotiate                = "Negotiate"
headerNameToString Opt                      = "Opt"
headerNameToString OrderingType             = "Ordering-Type"
headerNameToString Overwrite                = "Overwrite"
headerNameToString P3P                      = "P3P"
headerNameToString PEP                      = "PEP"
headerNameToString PICSLabel                = "PICS-Label"
headerNameToString PepInfo                  = "Pep-Info"
headerNameToString Position                 = "Position"
headerNameToString Pragma                   = "Pragma"
headerNameToString ProfileObject            = "Profile-Object"
headerNameToString Protocol                 = "Protocol"
headerNameToString ProtocolInfo             = "Protocol-Info"
headerNameToString ProtocolQuery            = "Protocol-Query"
headerNameToString ProtocolRequest          = "Protocol-Request"
headerNameToString ProxyAuthenticate        = "Proxy-Authenticate"
headerNameToString ProxyAuthenticationInfo  = "Proxy-AuthenticationInfo"
headerNameToString ProxyAuthorization       = "Proxy-Authorization"
headerNameToString ProxyFeatures            = "Proxy-Features"
headerNameToString ProxyInstruction         = "Proxy-Instruction"
headerNameToString Public                   = "Public"      
headerNameToString Range                    = "Range"          
headerNameToString Referer                  = "Referer"           
headerNameToString RetryAfter               = "Retry-After"


stringToHeaderName :: IsString a => a -> HeaderName
headerNameToString "AIM"                        = AIM              
headerNameToString "Accept"                     = Accept        
headerNameToString "Accept-Additions"           = AcceptAdditions
headerNameToString "Accept-Charset"             = Accept-Charset    
headerNameToString "Accept-Encoding"            = Accept-Encoding 
headerNameToString "Accept-Features"            = Accept-Features
headerNameToString "Accept-Language"            = Accept-Language
headerNameToString "Accept-Ranges"              = Accept-Ranges
headerNameToString "Age"                        = Age
headerNameToString "Allow"                      = Allow
headerNameToString "Alternates"                 = Alternates
headerNameToString "AuthenticationInfo"         = AuthenticationInfo
headerNameToString "Authorization"              = Authorization
headerNameToString "C-Ext"                      = C-Ext
headerNameToString "C-Man"                      = C-Man
headerNameToString "C-Opt"                      = C-Opt
headerNameToString "C-PEP"                      = C-PEP
headerNameToString "C-PEP-Info"                 = C-PEP-Info
headerNameToString "Cache-Control"              = Cache-Control
headerNameToString "Connection"                 = Connection
headerNameToString "Content-Base"               = ContentBase
headerNameToString "Content-Disposition"        = ContentDisposition
headerNameToString "Content-Encoding"           = ContentEncoding
headerNameToString "Content-ID"                 = ContentID
headerNameToString "Content-Language"           = ContentLanguage
headerNameToString "Content-Length"             = ContentLength
headerNameToString "Content-Location"           = ContentMD5
headerNameToString "Content-MD5"                = ContentMD5
headerNameToString "Content-Range"              = ContentRange
headerNameToString "Content-Script-Type"        = ContentScriptType
headerNameToString "Content-Style-Type"         = ContentStyleType
headerNameToString "Content-Type"               = ContentType
headerNameToString "Content-Version"            = ContentVersion
headerNameToString "Cookie"                     = Cookie
headerNameToString "Cookie2"                    = Cookie2
headerNameToString "DAV"                        = DAV
headerNameToString "Date"                       = Date
headerNameToString "Default-Style"              = DefaultStyle           
headerNameToString "Delta-Base"                 = DeltaBase
headerNameToString "Depth"                      = Depth
headerNameToString "Derived-From"               = DerivedFrom
headerNameToString "Destination"                = Destination
headerNameToString "Differential-ID"            = DifferentialID
headerNameToString "Digest"                     = Digest           
headerNameToString "E-Tag"                      = ETag
headerNameToString "Expect"                     = Expect
headerNameToString "Expires"                    = Expires
headerNameToString "Ext"                        = Ext
headerNameToString "From"                       = From
headerNameToString "Get-Profile"                = GetProfile
headerNameToString "Host"                       = Host
headerNameToString "IM"                         = IM
headerNameToString "If"                         = If
headerNameToString "If-Match"                   = IfMatch
headerNameToString "If-Modified-Since"          = IfModifiedSince
headerNameToString "If-None-Match"              = IfNoneMatch
headerNameToString "If-Range"                   = IfRange
headerNameToString "If-Unmodified-Since"        = IfUnmodifiedSince
headerNameToString "Keep-Alive"                 = KeepAlive
headerNameToString "Label"                      = Label
headerNameToString "Last-Modified"              = LastModified
headerNameToString "Link"                       = Link
headerNameToString "Location"                   = Location
headerNameToString "Lock-Token"                 = LockToken
headerNameToString "MIME-Version"               = MIMEVersion
headerNameToString "Man"                        = Man
headerNameToString "Max-Forwards"               = MaxForwards
headerNameToString "Meter"                      = Meter
headerNameToString "Negotiate"                  = Negotiate
headerNameToString "Opt"                        = Opt
headerNameToString "Ordering-Type"              = OrderingType
headerNameToString "Overwrite"                  = Overwrite
headerNameToString "P3P"                        = P3P
headerNameToString "PEP"                        = PEP
headerNameToString "PICS-Label"                 = PICSLabel
headerNameToString "Pep-Info"                   = PepInfo
headerNameToString "Position"                   = Position
headerNameToString "Pragma"                     = Pragma
headerNameToString "Profile-Object"             = Profile
headerNameToString "Protocol"                   = Object
headerNameToString "Protocol-Info"              = ProtocolInfo
headerNameToString "Protocol-Query"             = ProtocolQuery
headerNameToString "Protocol-Request"           = ProtocolRequest
headerNameToString "Proxy-Authenticate"         = ProxyAuthenticate
headerNameToString "Proxy-AuthenticationInfo"   = ProxyAuthenticationInfo
headerNameToString "Proxy-Authorization"        = ProxyAuthorization
headerNameToString "Proxy-Features"             = ProxyFeatures
headerNameToString "Proxy-Instruction"          = ProxyInstruction
headerNameToString "Public"                     = Public
headerNameToString "Range"                      = Range
headerNameToString "Referer"                    = Referer
headerNameToString "Retry-After"                = RetryAfter