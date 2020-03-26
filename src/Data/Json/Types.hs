module Data.Json.Types
  ( Json (..)
  ) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Vector as V

data Json
  = JsonNull
  | JsonFalse
  | JsonTrue
  | JsonNumber {-# UNPACK #-} !Int -- choose a more suitable type
  | JsonString                !BSL.ByteString
  | JsonList   {-# UNPACK #-} !(V.Vector Json)
  | JsonObject {-# UNPACK #-} !(V.Vector (BSL.ByteString, Json))
  deriving (Eq, Show)
