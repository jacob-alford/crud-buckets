module CrudBuckets.Helpers (showt) where

import Data.String (IsString, fromString)

showt :: (Show a, IsString b) => a -> b
showt = fromString . show