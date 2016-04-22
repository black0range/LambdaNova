module Smutt.HTTP.Error where


data ReaderError = HeaderTooLarge
                 | BadRequest String
                 | HTTPVersionNotSupported
                 | LengthRequired
                 | NotImplemented String
                 | BodyTooLarge
                   deriving (Show, Eq, Read)




