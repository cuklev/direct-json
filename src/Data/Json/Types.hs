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
  , jsonFieldIgnoreUnknown
  , jsonFieldInvalidUnknown
  , jsonFieldCaptureUnknown
  , (-:)
  , JsonFieldValues ((:+))
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
  jsonTypeParser = reverse <$> jsonAccumList (:) []

instance JsonType a => JsonType (V.Vector a) where
  jsonTypeParser = V.reverse . uncurry V.fromListN <$> jsonAccumList fold (0, [])
    where fold !x (!n, !xs) = (n+1, x:xs)

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

jsonAccumList :: JsonType a => (a -> b -> b) -> b -> Parser b
jsonAccumList fold initial = listFirst
  where
    listFirst = do
      char '['
      (initial <$ char ']') <|> loop initial

    loop !acc = do
      !x <- jsonTypeParser
      let !next = fold x acc
      anyChar >>= \case
        ']' -> pure next
        ',' -> loop next
        _   -> fail "Expected ',' or ']'"

data JsonFieldUnknown
  = JsonFieldIgnoreUnknown'
  | JsonFieldInvalidUnknown'
  | JsonFieldCaptureUnknown'

type CapturedFields x = [(BSL.ByteString, x)]

data JsonFieldParse (unknown :: JsonFieldUnknown) (fields :: [*]) where
  JsonFieldIgnoreUnknown  :: JsonFieldParse 'JsonFieldIgnoreUnknown' '[]
  JsonFieldInvalidUnknown :: JsonFieldParse 'JsonFieldInvalidUnknown' '[]
  JsonFieldCaptureUnknown :: JsonType x => !(CapturedFields x) -> JsonFieldParse 'JsonFieldCaptureUnknown' '[CapturedFields x]

  (:-:) :: JsonType x => !BSL.ByteString -> !(JsonFieldParse unknown xs) -> JsonFieldParse unknown (x ': xs)
  (:+:) :: !x -> !(JsonFieldParse unknown xs) -> JsonFieldParse unknown (x ': xs)

jsonFieldIgnoreUnknown :: JsonFieldParse 'JsonFieldIgnoreUnknown' '[]
jsonFieldIgnoreUnknown = JsonFieldIgnoreUnknown

jsonFieldInvalidUnknown :: JsonFieldParse 'JsonFieldInvalidUnknown' '[]
jsonFieldInvalidUnknown = JsonFieldInvalidUnknown

jsonFieldCaptureUnknown :: JsonType x => JsonFieldParse 'JsonFieldCaptureUnknown' '[CapturedFields x]
jsonFieldCaptureUnknown = JsonFieldCaptureUnknown []

(-:) :: JsonType x => BSL.ByteString -> JsonFieldParse unknown xs -> JsonFieldParse unknown (x ': xs)
(-:) = (:-:)

infixr -:

data JsonFieldValues fields where
  JsonFieldsEmpty :: JsonFieldValues '[]
  (:+) :: !x -> !(JsonFieldValues xs) -> JsonFieldValues (x ': xs)

infixr :+

class ExtractJsonFields unknown fields where
  extractFieldValues :: JsonFieldParse unknown fields -> Parser (JsonFieldValues fields)

instance ExtractJsonFields unknown '[] where
  extractFieldValues _ = pure JsonFieldsEmpty

instance ExtractJsonFields unknown xs => ExtractJsonFields unknown (x ': xs) where
  extractFieldValues (k :-: _)  = fail $ "Missing key " ++ show k
  extractFieldValues (x :+: xs) = (x :+) <$> extractFieldValues xs
  extractFieldValues (JsonFieldCaptureUnknown xs) = pure $ xs :+ JsonFieldsEmpty

jsonObjectField :: JsonFieldParse unknown fields -> Parser (JsonFieldParse unknown fields)
jsonObjectField fields = do
  !key <- jsonString
  char ':'

  let go :: JsonFieldParse unknown fields -> Parser (JsonFieldParse unknown fields)
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

jsonObject :: ExtractJsonFields unknown fields => JsonFieldParse unknown fields -> Parser (JsonFieldValues fields)
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
