{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
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
import Data.Bifunctor (second)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (isDigit, ord)
import Data.STRef
import Text.Json.Parser

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

type FieldParserList s = [(BSL.ByteString, Parser s ())]
newtype ObjectParserData s a = ObjectParserData (Parser s (FieldParserList s -> FieldParserList s, Parser s a))

instance Functor (ObjectParserData s) where
  fmap f (ObjectParserData obj) = ObjectParserData $ fmap (second (fmap f)) obj

instance Applicative (ObjectParserData s) where
  pure x = ObjectParserData $ pure (id, pure x)
  ObjectParserData mf <*> ObjectParserData mx
    = ObjectParserData $ do
      (f1, pf) <- mf
      (f2, px) <- mx
      pure ((f1 . f2), pf <*> px)

requiredField :: BSL.ByteString -> ValueParser s a -> ObjectParserData s a
requiredField key field = ObjectParserData $ do
  ref <- liftST $ newSTRef Nothing
  let storeValue = do
        !value <- valueParser field
        liftST (readSTRef ref) >>= \case
          Nothing -> liftST $ writeSTRef ref $ Just value
          Just _  -> fail $ "Duplicate key: " ++ show key

      getValue = liftST (readSTRef ref) >>= \case
        Nothing -> fail $ "Missing key: " ++ show key
        Just x -> pure x

  pure (((key, storeValue) :), getValue)

objectParser :: ObjectParserData s a -> Parser s a
objectParser (ObjectParserData obj) = do
  (makeFields, getValue) <- obj
  let fields = makeFields []

      parseField = do
        !key <- stringParser'
        case lookup key fields of
          Nothing -> fail $ "Unexpected key: " ++ show key
          Just parse -> do
            char ':'
            parse

      loop = anyChar >>= \case
        ',' -> do
          char '"'
          parseField
          loop
        '}' -> getValue
        _   -> fail "Expected ',' or '}'"

  anyChar >>= \case
    '}' -> getValue
    '"' -> do
      parseField
      loop
    _   -> fail "Expected '\"' or '}'"

decode :: (forall s. ValueParser s a) -> BSL.ByteString -> Either String a
decode parser input = runST $ runParser (valueParser parser) input
