module Smutt.HTTP.Server.Request.Content.Form where


import Data.Text.Lazy (Text)
import Data.HashMap.Strict
import Smutt.HTTP.Server.Request.Request
import Smutt.HTTP.Server.Request.Content.Error



type MaxSize = Int

readForm :: Request -> Maybe MaxSize -> IO (Either Error  [(Text,Text)]




readFormHS :: Request -> Maybe MaxSize -> IO (Either Error (HashMap Text Text))
