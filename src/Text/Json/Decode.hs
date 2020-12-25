{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
module Text.Json.Decode
  ( ParserList (..)
  , decode
  ) where

import Control.Monad.ST
import qualified Data.ByteString.Lazy as BSL
import Data.Traversable (for)
import Data.STRef
import Text.Json.Parser
import Unsafe.Coerce (unsafeCoerce)

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

  ArrayParser  :: ParserList b ys -> ((Int, [b]) -> a) -> ParserList a xs -> ParserList a ('ArrayParserType:xs)
  ObjectParser :: ObjectParserData a -> ParserList a xs -> ParserList a ('ObjectParserType:xs)

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
        | c == '"'  -> f <$> stringParser'
        | otherwise -> parse ps
      ArrayParser ys f ps
        | c == '['  -> f <$> arrayParser ys
        | otherwise -> parse ps
      ObjectParser parser ps
        | c == '{'  -> objectParser parser
        | otherwise -> parse ps

stringParser' :: Parser BSL.ByteString
stringParser' = takeWhileC (/= '"') <* char '"'

arrayParser :: ParserList a xs -> Parser (Int, [a])
arrayParser parserList = start
  where
    start = anyChar >>= \case
      ']' -> pure (0, [])
      c   -> do
        !x <- valueParser' c parserList
        loop 1 [x]

    loop !n !acc = anyChar >>= \case
      ',' -> do
        !x <- valueParser parserList
        loop (n+1) (x:acc)
      ']' -> pure (n, acc)
      _   -> fail "Expected ',' or ']'"

type family Placeholder :: * where {}

type FieldParserList s = [(BSL.ByteString, STRef s (Either (Parser Placeholder) Placeholder))]

data ObjectParserData a = ObjectParserData [(BSL.ByteString, Parser Placeholder)] (forall s. FieldParserList s -> Parser a)

instance Functor ObjectParserData where
  fmap f (ObjectParserData fields getValue) = ObjectParserData fields $ fmap f . getValue

instance Applicative ObjectParserData where
  pure x = ObjectParserData [] $ \_ -> pure x
  ObjectParserData fields1 getF <*> ObjectParserData fields2 getX
    = ObjectParserData (fields1 ++ fields2) $ \fields -> do
      f <- getF fields
      x <- getX fields
      pure $ f x

requiredField :: BSL.ByteString -> ParserList a xs -> ObjectParserData a
requiredField key parserList = ObjectParserData [(key, unsafeCoerce $ valueParser parserList)] getValue
  where
    getValue fields = do
      case lookup key fields of
        Nothing -> fail "not found"
        Just ref -> liftST (readSTRef $ unsafeCoerce ref) >>= \case
          Left _ -> fail "Field missing"
          Right x -> pure x

objectParser :: ObjectParserData a -> Parser a
objectParser (ObjectParserData fields getValue) = do
  finalFields <- for fields $ \(k, p) -> (k,) <$> liftST (newSTRef (Left p))

  let parseField = do
        !key <- stringParser'
        case lookup key fields of
          Nothing -> fail $ "Unexpected key " ++ show key
          Just ref -> liftST (readSTRef ref) >>= \case
            Right _ -> fail $ "Duplicate " ++ show key
            Left parser -> do
              char ':'
              !value <- parser
              liftST $ writeSTRef ref $ Right value

      loop = anyChar >>= \case
        ',' -> do
          char '"'
          parseField
          loop
        '}' -> getValue finalFields
        _   -> fail "Expected ',' or '}'"

  anyChar >>= \case
    '}' -> getValue finalFields
    '"' -> do
      parseField
      loop
    _   -> fail "Expected '\"' or '}'"

decode :: ParserList a xs -> BSL.ByteString -> Either String a
decode parserList input = runST $ runParser (valueParser parserList) input
