{-# LANGUAGE OverloadedStrings #-}

module CrudBuckets.Main (main) where

import Control.Concurrent (newMVar)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Logger.CallStack (MonadLogger, logDebug, logError, logInfo)
import CrudBuckets.Api (Api)
import CrudBuckets.Server (server)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as Text
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Servant.Server

loggingMiddleware :: Middleware
loggingMiddleware app req respond = do
  app req $ \res -> do
    runStdoutLoggingT $ logInfo $ "Request: " <> Text.pack (show req)
    respond res

main :: IO ()
main = do
  buckets <- newMVar mempty
  run 8080 (loggingMiddleware $ serve (Proxy :: Proxy Api) $ hoistServer (Proxy :: Proxy Api) runStdoutLoggingT $ server buckets)
