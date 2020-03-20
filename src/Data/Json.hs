module Data.Json
  ( JsonType (..)
  , JsonObject (..)
  , jsonObject
  , parseJson
  ) where

import qualified Data.ByteString.Lazy as BSL
import Data.Json.Parser (runParser)
import Data.Json.Types (JsonType (..), JsonObject (..), jsonObject)

parseJson :: JsonType a => BSL.ByteString -> Either String a
parseJson = runParser jsonTypeParser
