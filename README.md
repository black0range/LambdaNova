### Schmutt 
##### A simple webserver with performance in mind


***
To run a simple hello world program run 
```bash
ghc test.hs
./test
```
The test server is will be found on localhost:8000


To start a minimal server you need to import server and run serve with a handler thunk

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Server
import qualified Headers as H

main = serve (\x -> return $ FullResponse 200 [(H.ContentType, "text/html")] "<html><body><h1>Hello world!</h1><body></body>")
```
