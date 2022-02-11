module CrudBuckets.Main (main) where

import Control.Concurrent (newMVar)
import CrudBuckets.Api (Api)
import CrudBuckets.Server (server)
import Data.Proxy (Proxy (Proxy))
import Network.Wai.Handler.Warp (run)
import Servant.Server

main :: IO ()
main = do
  buckets <- newMVar mempty
  run 8080 (serve (Proxy :: Proxy Api) $ server buckets)
