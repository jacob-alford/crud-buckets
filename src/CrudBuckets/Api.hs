{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CrudBuckets.Api (Api, Schema, BucketName, BucketKey) where

import Data.Aeson (Value)
import Data.Aeson.Types (ToJSON)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant (FromHttpApiData)
import Servant.API

newtype BucketName = BucketName {unBucketName :: Text} deriving (ToJSON, FromHttpApiData, Eq, Ord, Show)

newtype BucketKey = BucketKey {unBucketKey :: Text} deriving (ToJSON, FromHttpApiData, Eq, Ord, Show)

type Api =
  -- curl -X GET http://localhost/users/userId-1
  Capture "bucket" BucketName -- --> "users"
    :> Capture "key" BucketKey -- --> "userId-1"
    :> Get '[JSON] Value
    :<|>
    -- curl -X PUT http://localhost/users/userId-1 --data-binary '<user-data>'
    Capture "bucket" BucketName -- --> "users"
      :> Capture "key" BucketKey -- --> "userId-1"
      :> ReqBody '[JSON] Value
      :> PutNoContent
    :<|>
    -- curl -X GET http://localhost/users
    Capture "bucket" BucketName -- --> "users"
      :> Get '[JSON] [BucketKey]
    :<|>
    -- curl -X PUT http://localhost/users --data-binary '<json-schema>'
    Capture "bucket" BucketName -- --> "users"
      :> ReqBody '[JsonSchemaCT] Schema
      :> PutNoContent

data JsonSchemaCT

type Schema = Value

instance MimeUnrender JsonSchemaCT Schema where
  mimeUnrender _ bs = mimeUnrender (Proxy :: Proxy JSON) bs

instance Accept JsonSchemaCT where
  contentType _ = "application/schema+json"