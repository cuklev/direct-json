{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
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
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import Data.Json.Parser
import Data.Json.Types
import Text.Read (readEither)

class JsonDecode a where
  jsonDecode :: Parser a

instance JsonDecode () where
  jsonDecode = jsonNull

instance JsonDecode Bool where
  jsonDecode = jsonBool

instance JsonDecode Int where
  jsonDecode = jsonInt

instance JsonDecode Double where
  jsonDecode = jsonDouble

instance JsonDecode BSL.ByteString where
  jsonDecode = jsonString

instance JsonDecode BS.ByteString where
  jsonDecode = fmap BSL.toStrict jsonString

instance JsonDecode T.Text where
  jsonDecode = fmap T.decodeUtf8 jsonDecode

instance JsonDecode a => JsonDecode [a] where
  jsonDecode = reverse . snd <$> jsonAccumList (:) []

instance JsonDecode a => JsonDecode (V.Vector a) where
  jsonDecode = V.reverse . uncurry V.fromListN <$> jsonAccumList (:) []

instance (JsonDecode a, VU.Unbox a) => JsonDecode (VU.Vector a) where
  jsonDecode = VU.reverse . uncurry VU.fromListN <$> jsonAccumList (:) []

instance (JsonDecode a, VS.Storable a) => JsonDecode (VS.Vector a) where
  jsonDecode = VS.reverse . uncurry VS.fromListN <$> jsonAccumList (:) []

instance JsonDecode Json where
  jsonDecode = selectOnFirstChar (fmap JsonNumber jsonDouble)
    [ ('n', JsonNull   <$  string "ull")
    , ('f', JsonFalse  <$  string "alse")
    , ('t', JsonTrue   <$  string "rue")
    , ('"', JsonString <$> jsonString')
    , ('[', list)
    , ('{', object)
    ]
    where
      list = JsonList . V.reverse . uncurry V.fromListN <$> jsonAccumList' (:) []
      object = do
        x :+ _ <- jsonObject' jsonFieldCaptureUnknown
        pure $ JsonObject $ V.fromList x

data Ignore = Ignore

instance JsonDecode Ignore where
  jsonDecode = selectOnFirstChar (Ignore <$ jsonDouble)
    [ ('n', Ignore <$ string "ull")
    , ('f', Ignore <$ string "alse")
    , ('t', Ignore <$ string "rue")
    , ('"', Ignore <$ jsonString')
    , ('[', Ignore <$ jsonAccumList' const Ignore)
    , ('{', Ignore <$ jsonObject' jsonFieldIgnoreUnknown)
    ]

jsonInt :: Parser Int
jsonInt = do
  negative <- (True <$ char '-') <|> pure False
  !digits <- takeWhileC1 isDigit
  let num = foldl' (\x d -> x*10 + ord d - ord '0') 0 $ BSL.unpack digits
  pure $ if negative then -num
                     else num

jsonDouble :: Parser Double
jsonDouble = do
  !digits <- takeWhileC1 (\c -> isDigit c || c == '-' || c == '.')
  either fail pure $ readEither $ BSL.unpack digits

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

jsonAccumList, jsonAccumList' :: JsonDecode a => (a -> b -> b) -> b -> Parser (Int, b)
jsonAccumList  fold initial = char '[' *> jsonAccumList' fold initial
jsonAccumList' fold initial = listFirst
  where
    listFirst = ((0, initial) <$ char ']') <|> loop 0 initial

    loop !n !acc = do
      !x <- atIndex n jsonDecode
      let !next = fold x acc
      anyChar >>= \case
        ']' -> pure (n+1, next)
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
      go JsonFieldIgnoreUnknown  = JsonFieldIgnoreUnknown <$ jsonDecode @Ignore
      go JsonFieldInvalidUnknown = fail $ "Unexpected key " ++ show key
      go (JsonFieldCaptureUnknown captured) = do
        !value <- jsonDecode
        pure $ JsonFieldCaptureUnknown ((key, value) : captured)
      go (x :-: xs)
        | key == x   = (:+: xs) <$> jsonDecode
        | otherwise  = (x :-:)  <$> go xs
      go (x :+: xs)  = (x :+:)  <$> go xs

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
