{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
module Text.Json.Decode
  ( parseNull
  , parseFalse
  , parseTrue
  , parseNumber
  , parseString
  , parseArray
  , parseObject
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

newtype ValueParser s a = ValueParser ((Char -> Parser s a) -> Char -> Parser s a)

instance Semigroup (ValueParser s a) where
  ValueParser p1 <> ValueParser p2 = ValueParser $ p1 . p2

parseNull :: a -> ValueParser s a
parseNull x = ValueParser $ \fallback -> \case
  'n' -> x <$ string "ull"
  c   -> fallback c

parseFalse :: a -> ValueParser s a
parseFalse x = ValueParser $ \fallback -> \case
  'f' -> x <$ string "alse"
  c   -> fallback c

parseTrue :: a -> ValueParser s a
parseTrue x = ValueParser $ \fallback -> \case
  't' -> x <$ string "rue"
  c   -> fallback c

parseNumber :: (Int -> a) -> ValueParser s a
parseNumber f = ValueParser $ \fallback -> \case
  '-' -> f . negate <$> (numberParser' =<< satisfy isDigit)
  c | isDigit c -> f <$> numberParser' c
    | otherwise -> fallback c

parseString :: (BSL.ByteString -> a) -> ValueParser s a
parseString f = ValueParser $ \fallback -> \case
  '"' -> f <$> stringParser'
  c   -> fallback c

parseArray :: ValueParser s b -> ((Int, [b]) -> a) -> ValueParser s a
parseArray single f = ValueParser $ \fallback -> \case
  '[' -> f <$> arrayParser single
  c   -> fallback c

parseObject :: ObjectParserData s a -> ValueParser s a
parseObject object = ValueParser $ \fallback -> \case
  '{' -> objectParser object
  c   -> fallback c

valueParser :: ValueParser s a -> Parser s a
valueParser parser = do
  c <- anyChar
  valueParser' c parser

valueParser' :: Char -> ValueParser s a -> Parser s a
valueParser' c (ValueParser p) = p (\_ -> fail $ "Unexpected " ++ show c) c

numberParser' :: Char -> Parser s Int
numberParser' c = do
  rest <- takeWhileC isDigit
  pure $ BSL.foldl' (\n d -> n * 10 + ord d - ord '0') (ord c - ord '0') rest

stringParser' :: Parser s BSL.ByteString
stringParser' = takeWhileC (/= '"') <* char '"'

arrayParser :: ValueParser s a -> Parser s (Int, [a])
arrayParser single = start
  where
    start = anyChar >>= \case
      ']' -> pure (0, [])
      c   -> do
        !x <- valueParser' c single
        loop 1 [x]

    loop !n !acc = anyChar >>= \case
      ',' -> do
        !x <- valueParser single
        loop (n+1) (x:acc)
      ']' -> pure (n, acc)
      _   -> fail "Expected ',' or ']'"

type family Placeholder :: * where {}

type FieldParserList s = [(BSL.ByteString, ValueParser s Placeholder)]
type FieldParserResultList s = [(BSL.ByteString, STRef s (Either (ValueParser s Placeholder) Placeholder))]

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

requiredField :: BSL.ByteString -> ValueParser s a -> ObjectParserData s a
requiredField key field = ObjectParserData ((key, unsafeCoerce field) :) getValue
  where
    getValue fields = do
      case lookup key fields of
        Nothing -> fail "not found"
        Just ref -> liftST (readSTRef $ unsafeCoerce ref) >>= \case
          Left _ -> fail "Field missing"
          Right x -> pure x

objectParser :: ObjectParserData s a -> Parser s a
objectParser (ObjectParserData makeFields getValue) = do
  finalFields <- liftST $ for (makeFields []) $ \(k, p) -> (k,) <$> newSTRef (Left p)

  let parseField = do
        !key <- stringParser'
        case lookup key finalFields of
          Nothing -> fail $ "Unexpected key " ++ show key
          Just ref -> liftST (readSTRef ref) >>= \case
            Right _ -> fail $ "Duplicate " ++ show key
            Left parser -> do
              char ':'
              !value <- valueParser parser
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

decode :: (forall s. ValueParser s a) -> BSL.ByteString -> Either String a
decode parser input = runST $ runParser (valueParser parser) input
