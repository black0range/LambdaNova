 
module Smutt.HTTP.Server.Request.Content.Error where




{- |
Any read opertion may fail at any time because of diffrent reasons
this data type is should contain any kind of error that a reading operation may give.
-} 
data ContentError =
    EOF
  | InvalidEncoding
  | ChunkSizeStingTooLarge
  | InvalidChunkSize
  | UnkownCharacterEncoding String
  | CustomError String
    deriving(Eq, Show, Read)
  

