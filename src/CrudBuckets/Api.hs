{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module CrudBuckets.Api (Api, Schema) where
import Servant.API
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Proxy (Proxy(Proxy))


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
    :> ReqBody '[JsonSchemaCT] Schema
    :> PutNoContent

data JsonSchemaCT

type Schema = Value

instance MimeUnrender JsonSchemaCT Schema where
  mimeUnrender _ bs = mimeUnrender (Proxy :: Proxy JSON) bs

instance Accept JsonSchemaCT where 
  contentType _ = "application/schema+json"