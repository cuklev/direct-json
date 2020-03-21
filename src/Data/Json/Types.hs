{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Json.Types
  ( JsonType (..)
  , jsonObject
  , JsonObject (JsonFieldIgnoreUnknown, JsonFieldInvalidUnknown, JsonFieldCaptureUnknown, (:-:))
  , JsonObjectValues ((:+))
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

data JsonFieldUnknown
  = JsonFieldIgnoreUnknown'
  | JsonFieldInvalidUnknown'
  | JsonFieldCaptureUnknown'

type CapturedFields x = [(BSL.ByteString, x)]

data JsonObject (unknown :: JsonFieldUnknown) (fields :: [*]) where
  JsonFieldIgnoreUnknown  :: JsonObject 'JsonFieldIgnoreUnknown' '[]
  JsonFieldInvalidUnknown :: JsonObject 'JsonFieldInvalidUnknown' '[]
  JsonFieldCaptureUnknown :: JsonType x => !(CapturedFields x) -> JsonObject 'JsonFieldCaptureUnknown' '[CapturedFields x]

  (:-:) :: JsonType x => !BSL.ByteString -> !(JsonObject unknown xs) -> JsonObject unknown (x ': xs)
  (:+:) :: !x -> !(JsonObject unknown xs) -> JsonObject unknown (x ': xs)

infixr :-:
infixr :+:

data JsonObjectValues fields where
  JsonObjectNoValues :: JsonObjectValues '[]
  (:+) :: !x -> !(JsonObjectValues xs) -> JsonObjectValues (x ': xs)

infixr :+

class ExtractJsonFields unknown fields where
  extractFieldValues :: JsonObject unknown fields -> Parser (JsonObjectValues fields)

instance ExtractJsonFields unknown '[] where
  extractFieldValues _ = pure JsonObjectNoValues

instance ExtractJsonFields unknown xs => ExtractJsonFields unknown (x ': xs) where
  extractFieldValues (k :-: _)  = fail $ "Missing key " ++ show k
  extractFieldValues (x :+: xs) = (x :+) <$> extractFieldValues xs
  extractFieldValues (JsonFieldCaptureUnknown xs) = pure $ xs :+ JsonObjectNoValues

jsonObjectField :: JsonObject unknown fields -> Parser (JsonObject unknown fields)
jsonObjectField fields = do
  !key <- jsonString
  char ':'

  let go :: JsonObject unknown fields -> Parser (JsonObject unknown fields)
      go JsonFieldIgnoreUnknown = JsonFieldIgnoreUnknown <$ fail "Ignoring is not implemented"
      go JsonFieldInvalidUnknown = fail $ "Unexpected key " ++ show key
      go (JsonFieldCaptureUnknown captured) = do
        !value <- jsonTypeParser
        pure $ JsonFieldCaptureUnknown ((key, value) : captured)
      go (x :-: xs)
        | key == x   = (:+: xs) <$> jsonTypeParser
        | otherwise  = (x :-:)  <$> go xs
      go (x :+: xs) = (x :+:)  <$> go xs

  go fields

jsonObject :: ExtractJsonFields unknown fields => JsonObject unknown fields -> Parser (JsonObjectValues fields)
jsonObject = objFirst
  where
    objFirst fields = do
      char '{'
      extractFieldValues =<< (fields <$ char '}') <|> loop fields

    loop fields = do
      !fields' <- jsonObjectField fields
      anyChar >>= \case
        '}' -> pure fields'
        ',' -> loop fields'
        _   -> fail "Expected ',' or '}'"
