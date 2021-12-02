module CrudBuckets.Server (server) where
import Servant.API
import Servant.Server
import Data.Aeson (Value)
import CrudBuckets.Api (Api)
import Data.Text (Text)

{-
  TODO
    1. Find someway to persist these buckets
      1. MVar State
    2. Rest of the routes
 -}

putSchema :: Text -> Value -> Handler NoContent
putSchema t v = pure NoContent

server :: Server Api
server = undefined :<|> undefined :<|> undefined :<|> putSchema
