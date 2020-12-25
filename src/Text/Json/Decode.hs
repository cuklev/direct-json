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
  , requiredField
  ) where

import Control.Monad.ST
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (isDigit, ord)
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
  ObjectParser :: (forall s. ObjectParserData s a) -> ParserList a xs -> ParserList a ('ObjectParserType:xs)

valueParser :: ParserList a xs -> Parser s a
valueParser parserList = do
  c <- anyChar
  valueParser' c parserList

valueParser' :: Char -> ParserList a xs -> Parser s a
valueParser' c = parse
  where
    parse :: ParserList a xs -> Parser s a
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
      NumberParser f ps
        | c == '-'  -> fail "Negative number parsing is not yet implemented"
        | isDigit c -> do
          rest <- takeWhileC isDigit
          pure $ f $ BSL.foldl' (\n d -> n * 10 + ord d - ord '0') (ord c - ord '0') rest
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

stringParser' :: Parser s BSL.ByteString
stringParser' = takeWhileC (/= '"') <* char '"'

arrayParser :: ParserList a xs -> Parser s (Int, [a])
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

type FieldParserList s = [(BSL.ByteString, Parser s Placeholder)]
type FieldParserResultList s = [(BSL.ByteString, STRef s (Either (Parser s Placeholder) Placeholder))]

data ObjectParserData s a = ObjectParserData (FieldParserList s -> FieldParserList s) (FieldParserResultList s -> Parser s a)

instance Functor (ObjectParserData s) where
  fmap f (ObjectParserData fields getValue) = ObjectParserData fields $ fmap f . getValue

instance Applicative (ObjectParserData s) where
  pure x = ObjectParserData id $ \_ -> pure x
  ObjectParserData fields1 getF <*> ObjectParserData fields2 getX
    = ObjectParserData (fields1 . fields2) $ \fields -> do
      f <- getF fields
      x <- getX fields
      pure $ f x

requiredField :: BSL.ByteString -> ParserList a xs -> ObjectParserData s a
requiredField key parserList = ObjectParserData ((key, unsafeCoerce $ valueParser parserList) :) getValue
  where
    getValue fields = do
      case lookup key fields of
        Nothing -> fail "not found"
        Just ref -> liftST (readSTRef $ unsafeCoerce ref) >>= \case
          Left _ -> fail "Field missing"
          Right x -> pure x

objectParser :: ObjectParserData s a -> Parser s a
objectParser (ObjectParserData makeFields getValue) = do
  finalFields <- for (makeFields []) $ \(k, p) -> (k,) <$> liftST (newSTRef (Left p))

  let parseField = do
        !key <- stringParser'
        case lookup key finalFields of
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
