{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , arrayElement
  , arrayEnd
  , arrayAny
  , arrayOf
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

parseArray :: ArrayParser s a -> ValueParser s a
parseArray single = ValueParser $ \fallback -> \case
  '[' -> skipWhile jsonWhitespace
      *> runArrayParser single
  c   -> fallback c

parseObject :: ObjectParser s a -> ValueParser s a
parseObject (ObjectParser obj) = ValueParser $ \fallback -> \case
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
           <> parseArray ignoreArray
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

newtype ArrayParser s a = ArrayParser { runArrayParser :: Parser s a }
newtype ArrayParser1 s a = ArrayParser1 { runArrayParser1 :: Int -> Parser s a }

instance Functor (ArrayParser s) where
  fmap f (ArrayParser p) = ArrayParser $ fmap f p

class ArrayParserClass ap where
  arrayEnd     :: a -> ap s a
  arrayElement :: ValueParser s b -> (b -> ArrayParser1 s a) -> ap s a
  arrayAny     :: a -> ValueParser s b -> (b -> ArrayParser1 s a) -> ap s a

instance ArrayParserClass ArrayParser where
  arrayEnd x = ArrayParser $ x <$ char ']'

  arrayElement single next = ArrayParser $ do
    !x <- atArrayIndex 0 $ valueParser single
    runArrayParser1 (next x) 1

  arrayAny end single next = ArrayParser $ anyChar >>= \case
    ']' -> pure end
    c   -> do
      !x <- atArrayIndex 0 $ valueParser' c single
      runArrayParser1 (next x) 1

instance ArrayParserClass ArrayParser1 where
  arrayEnd x = ArrayParser1 $ \(!_) -> x <$ char ']'

  arrayElement single next = ArrayParser1 $ \(!n) -> do
    char ','
    skipWhile jsonWhitespace
    !x <- atArrayIndex n $ valueParser single
    runArrayParser1 (next x) (n + 1)

  arrayAny end single next = ArrayParser1 $ \(!n) -> anyChar >>= \case
    ',' -> do
      skipWhile jsonWhitespace
      !x <- atArrayIndex n $ valueParser single
      runArrayParser1 (next x) (n + 1)
    ']' -> pure end
    _   -> fail "Expected ',' or ']'"

ignoreArray :: ArrayParser s ()
ignoreArray = go
  where
    go :: ArrayParserClass ap => ap s ()
    go = arrayAny () parseIgnore $ \() -> go

arrayOf :: forall s a. ValueParser s a -> ArrayParser s [a]
arrayOf single = go []
  where
    go :: ArrayParserClass ap => [a] -> ap s [a]
    go !xs = arrayAny (reverse xs) single $ \x -> go (x:xs)

type FieldParser s = BSL.ByteString -> Parser s ()
newtype ObjectParser s a = ObjectParser (Parser s (FieldParser s -> FieldParser s, Parser s a))

instance Functor (ObjectParser s) where
  fmap f (ObjectParser obj) = ObjectParser $ fmap (second (fmap f)) obj

instance Applicative (ObjectParser s) where
  pure x = ObjectParser $ pure (id, pure x)
  ObjectParser mf <*> ObjectParser mx
    = ObjectParser $ do
      (f1, pf) <- mf
      (f2, px) <- mx
      pure (f1 . f2, pf <*> px)

someField :: (Maybe a -> Parser s b) -> BSL.ByteString -> ValueParser s a -> ObjectParser s b
someField modify key field = ObjectParser $ do
  ref <- liftST $ newSTRef Nothing
  let storeValue fallback k
        | k /= key = fallback k
        | otherwise = do
          !value <- valueParser field
          liftST $ writeSTRef ref $ Just value

      getValue = modify =<< liftST (readSTRef ref)

  pure (storeValue, getValue)

requiredField :: BSL.ByteString -> ValueParser s a -> ObjectParser s a
requiredField key = someField (maybe missing pure) key
  where missing = fail $ "Missing key: " ++ show key

optionalField :: BSL.ByteString -> ValueParser s a -> ObjectParser s (Maybe a)
optionalField = someField pure

ignoreAnyField :: ObjectParser s ()
ignoreAnyField = ObjectParser $ do
  let storeValue _ _ = valueParser parseIgnore
      getValue = pure ()
  pure (storeValue, getValue)

captureFields :: ValueParser s a -> ObjectParser s [(BSL.ByteString, a)]
captureFields single = ObjectParser $ do
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
