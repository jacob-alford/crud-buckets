module CrudBuckets.Main (main) where

import Data.Proxy (Proxy (Proxy))
import Servant.Server
import Network.Wai.Handler.Warp (run)

import CrudBuckets.Api (Api)
import CrudBuckets.Server (server)

main :: IO ()
main = run 8080 (serve (Proxy :: Proxy Api) server)
