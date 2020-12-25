{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Json.Decode
  ( ParserList (..)
  , decode
  ) where

import qualified Data.ByteString.Lazy as BSL
import Control.Monad.ST
import Text.Json.Parser

data ParserType
  = NullParserType
  | FalseParserType
  | TrueParserType
  | NumberParserType
  | StringParserType
  | ArrayParserType
  | ObjectParserType

data ParserList a (xs :: [ParserType]) where
  Empty        :: ParserList a '[]

  NullParser   :: a -> ParserList a xs -> ParserList a ('NullParserType:xs)
  FalseParser  :: a -> ParserList a xs -> ParserList a ('FalseParserType:xs)
  TrueParser   :: a -> ParserList a xs -> ParserList a ('TrueParserType:xs)

  NumberParser :: (Int -> a) -> ParserList a xs -> ParserList a ('NumberParserType:xs)
  StringParser :: (BSL.ByteString -> a) -> ParserList a xs -> ParserList a ('StringParserType:xs)

  ArrayParser  :: ParserList a xs -> ParserList a ('ArrayParserType:xs)
  ObjectParser :: ParserList a xs -> ParserList a ('ObjectParserType:xs)

valueParser :: ParserList a xs -> Parser a
valueParser parserList = do
  c <- anyChar
  valueParser' c parserList

valueParser' :: Char -> ParserList a xs -> Parser a
valueParser' c = parse
  where
    parse :: ParserList a xs -> Parser a
    parse = \case
      Empty -> fail $ "Unexpected " ++ show c
      NullParser x ps
        | c == 'n'  -> x <$ string "ull"
        | otherwise -> parse ps
      FalseParser x ps
        | c == 'f'  -> x <$ string "alse"
        | otherwise -> parse ps
      TrueParser x ps
        | c == 't'  -> x <$ string "rue"
        | otherwise -> parse ps
      NumberParser _ ps
        | c == '-' || ('0' <= c && c <= '9') -> fail "Number parsing is not yet implemented"
        | otherwise -> parse ps
      StringParser f ps
        | c == '"'  -> f <$> takeWhileC (/= '"') <* char '"'
        | otherwise -> parse ps
      ArrayParser ps
        | c == '['  -> fail "Array parsing is not yet implemented"
        | otherwise -> parse ps
      ObjectParser ps
        | c == '{'  -> fail "Object parsing is not yet implemented"
        | otherwise -> parse ps

decode :: ParserList a xs -> BSL.ByteString -> Either String a
decode parserList input = runST $ runParser (valueParser parserList) input
