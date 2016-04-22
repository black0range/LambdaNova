module Smutt.HTTP.Server.Request.Data.Text where



import Smutt.HTTP.Server.Request.Request
import Smutt.HTTP.Server.Request.Content.Error 


type MaxSize = Int 
readText :: Request -> MaxSize Int ->  IO (Either Error Text)

