{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Json.Types
  ( JsonType (..)
  , JsonObject (..)
  , jsonObject
  ) where

import Control.Applicative ((<|>))
import Data.Char (ord, isDigit)
import Data.List (foldl')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Data.Json.Parser

class JsonType a where
  jsonTypeParser :: Parser a

instance JsonType () where
  jsonTypeParser = jsonNull

instance JsonType Bool where
  jsonTypeParser = jsonBool

instance JsonType Int where
  jsonTypeParser = jsonInt

instance JsonType BSL.ByteString where
  jsonTypeParser = jsonString

instance JsonType BS.ByteString where
  jsonTypeParser = fmap BSL.toStrict jsonString

instance JsonType T.Text where
  jsonTypeParser = fmap T.decodeUtf8 jsonTypeParser

instance JsonType a => JsonType [a] where
  jsonTypeParser = jsonListOf jsonTypeParser

instance JsonType a => JsonType (V.Vector a) where
  jsonTypeParser = jsonListOf' jsonTypeParser

jsonInt :: Parser Int
jsonInt = do
  !digits <- takeWhileC1 isDigit
  pure $ foldl' (\x d -> x*10 + ord d - ord '0') 0 $ BSL.unpack digits

jsonString :: Parser BSL.ByteString
jsonString = char '"' *> takeWhileC (/= '"') <* anyChar

jsonNull :: Parser ()
jsonNull = string "null"

jsonBool :: Parser Bool
jsonBool = do
  c <- anyChar
  case c of
    't' -> True  <$ string "rue"
    'f' -> False <$ string "alse"
    _   -> fail "Expected a boolean"

jsonListOf :: Parser a -> Parser [a]
jsonListOf p = do
  char '['
  ([] <$ char ']') <|> ((:) <$> p <*> loop)

  where
    loop = anyChar >>= \case
      ']' -> pure []
      ',' -> do
        !x <- p
        (x:) <$> loop
      _   -> fail "Expected ',' or ']'"

jsonListOf' :: Parser a -> Parser (V.Vector a)
jsonListOf' p = do
  char '['
  (V.empty <$ char ']') <|> loop 0 []

  where
    loop !n !acc = do
      !x <- p
      anyChar >>= \case
        ']' -> pure $ V.reverse $ V.fromListN (n+1) (x:acc)
        ',' -> loop (n+1) (x:acc)
        _   -> fail "Expected ',' or ']'"

data JsonObject fields where
  JsonObjectIgnoreField :: JsonObject '[]
  JsonObjectInvalidField :: JsonObject '[]
  JsonObjectCapture :: JsonType x => ![(BSL.ByteString, x)] -> JsonObject '[x]
  (:--:) :: JsonType x => !BSL.ByteString -> !(JsonObject xs) -> JsonObject (x ': xs)
  (:++:) :: !x -> JsonObject xs -> JsonObject (x ': xs)

infixr :++:
infixr :--:

jsonObjectField :: JsonObject fields -> Parser (JsonObject fields)
jsonObjectField fields = do
  !key <- jsonString
  char ':'

  let go :: JsonObject fields -> Parser (JsonObject fields)
      go JsonObjectIgnoreField = JsonObjectIgnoreField <$ fail "Ignoring is not implemented"
      go JsonObjectInvalidField = fail $ "Unexpected key " ++ show key
      go (JsonObjectCapture captured) = do
        !value <- jsonTypeParser
        pure $ JsonObjectCapture ((key, value) : captured)
      go (x :--: xs)
        | key == x   = (:++: xs) <$> jsonTypeParser
        | otherwise  = (x :--:)  <$> go xs
      go (x :++: xs) = (x :++:)  <$> go xs

  go fields

jsonObject :: JsonObject fields -> Parser (JsonObject fields)
jsonObject = objFirst
  where
    objFirst fields = do
      char '{'
      (fields <$ char '}') <|> loop fields

    loop fields = do
      !fields' <- jsonObjectField fields
      anyChar >>= \case
        '}' -> pure fields'
        ',' -> loop fields'
        _   -> fail "Expected ',' or '}'"
