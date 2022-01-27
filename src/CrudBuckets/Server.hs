module CrudBuckets.Server (server) where
import Control.Concurrent
import Control.Monad.IO.Class
import Servant.API
import Servant.Server
import Data.Aeson (Value)
import CrudBuckets.Api (Api, Schema)
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map

data Bucket = Bucket {
    schema :: Schema,
    values :: Map Text Value
  }

newtype Buckets = Buckets {
  getBuckets :: Map Text Bucket
}

{-
  TODO
    1. Find someway to persist these buckets
      1. MVar State
    2. Rest of the routes
 -}

updateIfEmpty :: Buckets -> Text -> Schema -> Maybe Buckets
updateIfEmpty buckets key schema = 
  case Map.lookup key $ getBuckets buckets of
    Nothing -> Just $ Buckets $ Map.insert t bucket (getBuckets a)
    Just _ -> Nothing

putSchema :: MVar Buckets -> Text -> Schema -> Handler NoContent
putSchema buckets t v = do 
  let bucket = Bucket { schema = v, values = mempty }
  let failOrNot = do
    buckets <- buckets
  liftIO $ modifyMVar_ buckets (\a -> pure $ updateIfEmpty)
  pure NoContent

server :: Server Api
server = undefined :<|> undefined :<|> undefined :<|> putSchema undefined
