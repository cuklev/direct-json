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
  , parseIgnore
  , decode
  , requiredField
  , optionalField
  , ignoreAnyField
  , captureFields
  ) where

import Control.Monad.ST
import Data.Bifunctor (first, second)
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
  '0' -> pure $ f 0
  c | isDigit c -> f <$> numberParser' c
    | otherwise -> fallback c

parseString :: (BSL.ByteString -> a) -> ValueParser s a
parseString f = ValueParser $ \fallback -> \case
  '"' -> f <$> stringParser'
  c   -> fallback c

parseArray :: ValueParser s b -> ((Int, [b]) -> a) -> ValueParser s a
parseArray single f = ValueParser $ \fallback -> \case
  '[' -> skipWhile jsonWhitespace
      *> (f <$> arrayParser single)
  c   -> fallback c

parseObject :: ObjectParserData s a -> ValueParser s a
parseObject (ObjectParserData obj) = ValueParser $ \fallback -> \case
  '{' -> do
    skipWhile jsonWhitespace
    (makeStoreValue, getValue) <- obj
    let storeValue = makeStoreValue $ \key -> fail $ "Unexpected key: " ++ show key

        parseField = do
          !key <- stringParser'
          skipWhile jsonWhitespace
          char ':'
          skipWhile jsonWhitespace
          atObjectKey key $ storeValue key

        loop = anyChar >>= \case
          ',' -> do
            skipWhile jsonWhitespace
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

  c   -> fallback c

-- |Parses any valid json value and ignores it
parseIgnore :: ValueParser s ()
parseIgnore = parseNull ()
           <> parseFalse ()
           <> parseTrue ()
           <> parseNumber (const ())
           <> parseString (const ())
           <> parseArray parseIgnore (const ())
           <> parseObject ignoreAnyField

valueParser :: ValueParser s a -> Parser s a
valueParser parser = do
  c <- anyChar
  valueParser' c parser

valueParser' :: Char -> ValueParser s a -> Parser s a
valueParser' c (ValueParser p) = p (\_ -> fail $ "Unexpected " ++ show c) c
                              <* skipWhile jsonWhitespace

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
        !x <- atArrayIndex 0 $ valueParser' c single
        loop 1 [x]

    loop !n !acc = anyChar >>= \case
      ',' -> do
        skipWhile jsonWhitespace
        !x <- atArrayIndex n $ valueParser single
        loop (n+1) (x:acc)
      ']' -> pure (n, acc)
      _   -> fail "Expected ',' or ']'"

type FieldParser s = BSL.ByteString -> Parser s ()
newtype ObjectParserData s a = ObjectParserData (Parser s (FieldParser s -> FieldParser s, Parser s a))

instance Functor (ObjectParserData s) where
  fmap f (ObjectParserData obj) = ObjectParserData $ fmap (second (fmap f)) obj

instance Applicative (ObjectParserData s) where
  pure x = ObjectParserData $ pure (id, pure x)
  ObjectParserData mf <*> ObjectParserData mx
    = ObjectParserData $ do
      (f1, pf) <- mf
      (f2, px) <- mx
      pure (f1 . f2, pf <*> px)

someField :: (Maybe a -> Parser s b) -> BSL.ByteString -> ValueParser s a -> ObjectParserData s b
someField modify key field = ObjectParserData $ do
  ref <- liftST $ newSTRef Nothing
  let storeValue fallback k
        | k /= key = fallback k
        | otherwise = do
          !value <- valueParser field
          liftST (readSTRef ref) >>= \case
            Nothing -> liftST $ writeSTRef ref $ Just value
            Just _  -> fail $ "Duplicate key: " ++ show key

      getValue = modify =<< liftST (readSTRef ref)

  pure (storeValue, getValue)

requiredField :: BSL.ByteString -> ValueParser s a -> ObjectParserData s a
requiredField key = someField (maybe missing pure) key
  where missing = fail $ "Missing key: " ++ show key

optionalField :: BSL.ByteString -> ValueParser s a -> ObjectParserData s (Maybe a)
optionalField = someField pure

ignoreAnyField :: ObjectParserData s ()
ignoreAnyField = ObjectParserData $ do
  let storeValue _ _ = valueParser parseIgnore
      getValue = pure ()
  pure (storeValue, getValue)

captureFields :: ValueParser s a -> ObjectParserData s [(BSL.ByteString, a)]
captureFields single = ObjectParserData $ do
  ref <- liftST $ newSTRef []
  let storeValue _ key = do
        !value <- valueParser single
        liftST $ modifySTRef' ref ((key, value) :)
      getValue = liftST (readSTRef ref)
  pure (storeValue, getValue)

decode :: (forall s. ValueParser s a) -> BSL.ByteString -> Either String a
decode parser input = first showError $ runST $ runParser (valueParser parser <* skipWhile jsonWhitespace) $ BSL.dropWhile jsonWhitespace input

jsonWhitespace :: Char -> Bool
jsonWhitespace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'
