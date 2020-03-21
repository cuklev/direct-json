module Data.Json
  ( parseJson
  , module Data.Json.Types
  ) where

import qualified Data.ByteString.Lazy as BSL
import Data.Json.Parser (runParser)
import Data.Json.Types

parseJson :: JsonType a => BSL.ByteString -> Either String a
parseJson = runParser jsonTypeParser
