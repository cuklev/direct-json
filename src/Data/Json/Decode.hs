{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnboxedSums #-}
module Data.Json.Decode
  ( Json (..)
  , JsonDecode (..)
  , jsonObject
  , jsonFieldIgnoreUnknown
  , jsonFieldInvalidUnknown
  , jsonFieldCaptureUnknown
  , (-:)
  , JsonFieldValues ((:+))
  , decode
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
import Data.Json.Types

class JsonDecode a where
  jsonDecode :: Parser a

instance JsonDecode () where
  jsonDecode = jsonNull

instance JsonDecode Bool where
  jsonDecode = jsonBool

instance JsonDecode Int where
  jsonDecode = jsonInt

instance JsonDecode BSL.ByteString where
  jsonDecode = jsonString

instance JsonDecode BS.ByteString where
  jsonDecode = fmap BSL.toStrict jsonString

instance JsonDecode T.Text where
  jsonDecode = fmap T.decodeUtf8 jsonDecode

instance JsonDecode a => JsonDecode [a] where
  jsonDecode = reverse <$> jsonAccumList (:) []

instance JsonDecode a => JsonDecode (V.Vector a) where
  jsonDecode = V.reverse . uncurry V.fromListN <$> jsonAccumList fold (0, [])
    where fold !x (!n, !xs) = (n+1, x:xs)

instance JsonDecode Json where
  jsonDecode = selectOnFirstChar (fmap JsonNumber jsonInt)
    [ ('n', JsonNull   <$  string "ull")
    , ('f', JsonFalse  <$  string "alse")
    , ('t', JsonTrue   <$  string "rue")
    , ('"', JsonString <$> jsonString')
    , ('[', list)
    , ('{', object)
    ]
    where
      fold !x (!n, !xs) = (n+1, x:xs)
      list = JsonList . V.reverse . uncurry V.fromListN <$> jsonAccumList' fold (0, [])
      object = do
        x :+ _ <- jsonObject' jsonFieldCaptureUnknown
        pure $ JsonObject $ V.fromList x

jsonInt :: Parser Int
jsonInt = do
  negative <- (True <$ char '-') <|> pure False
  !digits <- takeWhileC1 isDigit
  let num = foldl' (\x d -> x*10 + ord d - ord '0') 0 $ BSL.unpack digits
  pure $ if negative then -num
                     else num

jsonString, jsonString' :: Parser BSL.ByteString
jsonString  = char '"' *> jsonString'
jsonString' = takeWhileC (/= '"') <* anyChar

jsonNull :: Parser ()
jsonNull = string "null"

jsonBool :: Parser Bool
jsonBool = selectOnFirstChar (fail "Expected a boolean")
  [ ('f', False <$ string "alse")
  , ('t', True  <$ string "rue")
  ]

jsonAccumList, jsonAccumList' :: JsonDecode a => (a -> b -> b) -> b -> Parser b
jsonAccumList  fold initial = char '[' *> jsonAccumList' fold initial
jsonAccumList' fold initial = listFirst
  where
    listFirst = (initial <$ char ']') <|> loop 0 initial

    loop !n !acc = do
      !x <- atIndex n jsonDecode
      let !next = fold x acc
      anyChar >>= \case
        ']' -> pure next
        ',' -> loop (n+1) next
        _   -> fail "Expected ',' or ']'"

data JsonFieldUnknown
  = JsonFieldIgnoreUnknown'
  | JsonFieldInvalidUnknown'
  | JsonFieldCaptureUnknown'

type CapturedFields x = [(BSL.ByteString, x)]

data JsonFieldParse (unknown :: JsonFieldUnknown) (fields :: [*]) where
  JsonFieldIgnoreUnknown  :: JsonFieldParse 'JsonFieldIgnoreUnknown' '[]
  JsonFieldInvalidUnknown :: JsonFieldParse 'JsonFieldInvalidUnknown' '[]
  JsonFieldCaptureUnknown :: JsonDecode x => !(CapturedFields x) -> JsonFieldParse 'JsonFieldCaptureUnknown' '[CapturedFields x]

  (:-:) :: JsonDecode x => !BSL.ByteString -> !(JsonFieldParse unknown xs) -> JsonFieldParse unknown (x ': xs)
  (:+:) :: !x -> !(JsonFieldParse unknown xs) -> JsonFieldParse unknown (x ': xs)

jsonFieldIgnoreUnknown :: JsonFieldParse 'JsonFieldIgnoreUnknown' '[]
jsonFieldIgnoreUnknown = JsonFieldIgnoreUnknown

jsonFieldInvalidUnknown :: JsonFieldParse 'JsonFieldInvalidUnknown' '[]
jsonFieldInvalidUnknown = JsonFieldInvalidUnknown

jsonFieldCaptureUnknown :: JsonDecode x => JsonFieldParse 'JsonFieldCaptureUnknown' '[CapturedFields x]
jsonFieldCaptureUnknown = JsonFieldCaptureUnknown []

(-:) :: JsonDecode x => BSL.ByteString -> JsonFieldParse unknown xs -> JsonFieldParse unknown (x ': xs)
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
        !value <- jsonDecode
        pure $ JsonFieldCaptureUnknown ((key, value) : captured)
      go (x :-: xs)
        | key == x   = (:+: xs) <$> jsonDecode
        | otherwise  = (x :-:)  <$> go xs
      go (x :+: xs) = (x :+:)  <$> go xs

  atKey key $ go fields

jsonObject, jsonObject' :: ExtractJsonFields unknown fields => JsonFieldParse unknown fields -> Parser (JsonFieldValues fields)
jsonObject fields = char '{' *> jsonObject' fields
jsonObject' = objFirst
  where
    objFirst fields =
      extractFieldValues =<< (fields <$ char '}') <|> loop fields

    loop fields = do
      !fields' <- jsonObjectField fields
      anyChar >>= \case
        '}' -> pure fields'
        ',' -> loop fields'
        _   -> fail "Expected ',' or '}'"

decode :: JsonDecode a => BSL.ByteString -> Either String a
decode = runParser jsonDecode
