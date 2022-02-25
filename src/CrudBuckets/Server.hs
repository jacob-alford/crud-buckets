{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CrudBuckets.Server (server) where

import Control.Concurrent
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import CrudBuckets.Api (Api, Schema)
import Data.Aeson (Value)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Servant.API
import Servant.Server

data Bucket = Bucket
  { schema :: Schema,
    values :: Map Text Value
  }

newtype Buckets = Buckets
  { unBuckets :: Map Text Bucket
  }
  deriving newtype (Semigroup, Monoid)

{-
  TODO
    1. --Find someway to persist these buckets--
      1. --MVar State--
    2. --Rest of the routes--
      1. Validate schema
      2. Validate body-values with schema
    3. Logging
    4. Switch to Servant-Generics
    5. Persist MVar to disk
 -}

updateIfEmpty :: Buckets -> Text -> Schema -> Maybe Buckets
updateIfEmpty buckets key schema =
  let bucket = Bucket {schema = schema, values = mempty}
   in case Map.lookup key $ unBuckets buckets of
        Nothing -> Just $ Buckets $ Map.insert key bucket (unBuckets buckets)
        Just _ -> Nothing

putSchema :: MVar Buckets -> Text -> Schema -> Handler NoContent
putSchema buckets key schema = do
  updated <- liftIO $ modifyMVar buckets updateBuckets
  if updated then pure NoContent else throwError err409
  where
    updateBuckets :: Buckets -> IO (Buckets, Bool)
    updateBuckets buckets = case updateIfEmpty buckets key schema of
      Just newBuckets -> pure (newBuckets, True)
      Nothing -> pure (buckets, False)

putValue :: MVar Buckets -> Text -> Text -> Value -> Handler NoContent
putValue buckets bucketKey key jsonValue = do
  updated <- liftIO $ modifyMVar buckets updateBuckets
  if updated then pure NoContent else throwError err404
  where
    updateBuckets :: Buckets -> IO (Buckets, Bool)
    updateBuckets buckets = case Map.lookup bucketKey (unBuckets buckets) of
      Nothing -> pure (buckets, False)
      Just bucket -> do
        let newBucket = bucket {values = Map.insert key jsonValue (values bucket)}
        pure (Buckets $ Map.insert bucketKey newBucket (unBuckets buckets), True)

getValue :: MVar Buckets -> Text -> Text -> Handler Value
getValue mvarBuckets bucketKey key = do
  buckets <- liftIO $ readMVar mvarBuckets
  case Map.lookup bucketKey (unBuckets buckets) of
    Nothing -> throwError err404
    Just bucket -> case Map.lookup key (values bucket) of
      Nothing -> throwError err404
      Just value -> pure value

getBucketKeys :: MVar Buckets -> Text -> Handler [Text]
getBucketKeys mvarBuckets bucketKey = do
  buckets <- liftIO $ readMVar mvarBuckets
  case Map.lookup bucketKey (unBuckets buckets) of
    Nothing -> throwError err404
    Just bucket -> pure $ Map.keys (values bucket)

server :: MVar Buckets -> Server Api
server buckets = getValue buckets :<|> putValue buckets :<|> getBucketKeys buckets :<|> putSchema buckets
