module CrudBuckets.Api (Api) where
import Servant.API
import Data.Aeson (Value)


type Api =
    -- curl -X GET http://localhost/users/userId-1
    Capture "bucket" Text -- --> "users"
    :> Capture "key" Text -- --> "userId-1"
    :> Get '[JSON] Value
  :<|>
    -- curl -X PUT http://localhost/users/userId-1 --data-binary '<user-data>'
    Capture "bucket" Text -- --> "users"
    :> Capture "key" Text -- --> "userId-1"
    :> ReqBody '[JSON] Value
    :> PutNoContent
  :<|>
    -- curl -X GET http://localhost/users
    Capture "bucket" Text -- --> "users"
    :> Get '[JSON] [Text]
  :<|>
    -- curl -X PUT http://localhost/users --data-binary '<json-schema>'
    Capture "bucket" Text -- --> "users"
    :> ReqBody '[JsonSchemaCT] JsonSchema
    :> PutNoContent

