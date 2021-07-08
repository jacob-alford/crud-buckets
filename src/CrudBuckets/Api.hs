module CrudBuckets.Api (Api) where
import Servant.API

data Comment = Comment {
  id :: String,
  body :: String,
  user :: User,
  user_id :: String,
  parent_id :: Maybe String,
  parent_comment :: Maybe Comment,
  child_comments :: [Comment],
  post_id :: String
                       }

data User = User {
  id :: String,
  email :: String,
  password :: String,
  display_name :: String,
  curernt_refresh_token :: Maybe String,
  comments :: [Comment]
                 }

type Api = "users" :> Get '[JSON] [User]
