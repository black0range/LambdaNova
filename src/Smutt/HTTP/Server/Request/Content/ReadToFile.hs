module Smutt.HTTP.Server.Request.Contet.ReadToFile where


import Smutt.HTTP.Server.Request.Request 
import Smutt.HTTP.Server.Request.Content.Error


import Control.Monad.Except


-- | Reads the content of a request to a file. Will overwrite any existing file or create one
-- if a file doesn't exist yet. If the MaxSize is exceedet it will delete the file and throw
-- an error 

type MaxSize = Int 
readToFile :: Request -> FilePath -> Maybe MaxSize -> ExceptT ContentError IO FilePath



