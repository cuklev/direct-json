{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
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
  , validateValue
  , requiredElement
  , optionalElement
  , arrayOf
  , requiredField
  , optionalField
  , ignoreAnyField
  , captureFields
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.ST
import Data.Bifunctor (first, second)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (isDigit, ord, chr)
import Data.Scientific (Scientific, scientific)
import Data.STRef
import Text.Json.Parser

newtype ValueParser s a = ValueParser (Char -> Maybe (Parser s a))

instance Semigroup (ValueParser s a) where
  ValueParser p1 <> ValueParser p2 = ValueParser $ \c -> p1 c <|> p2 c

validateValue :: (a -> Either String b) -> ValueParser s a -> ValueParser s b
validateValue f (ValueParser vp) = ValueParser $ fmap (either fail pure . f =<<) . vp

instance Functor (ValueParser s) where
  fmap f = validateValue $ Right . f

parseNull :: ValueParser s ()
parseNull = ValueParser $ \case
  'n' -> Just $ string "ull"
  _   -> Nothing

parseFalse :: ValueParser s ()
parseFalse = ValueParser $ \case
  'f' -> Just $ string "alse"
  _   -> Nothing

parseTrue :: ValueParser s ()
parseTrue = ValueParser $ \case
  't' -> Just $ string "rue"
  _   -> Nothing

parseNumber :: ValueParser s Scientific
parseNumber = ValueParser $ \case
  '-' -> Just $ uncurry scientific . first negate <$> (number =<< satisfy isDigit)
  c | isDigit c -> Just $ uncurry scientific <$> number c
    | otherwise -> Nothing
  where
    number :: Char -> Parser s (Integer, Int)
    number '0' = pure (0, 0)
    number c = do
      n1 <- addDigits (toInteger $ ord c - ord '0') <$> takeWhileC isDigit
      (n2, e1) <- takeIf (== '.') >>= \case
        Nothing -> pure (n1, 0)
        Just _  -> do
          frac <- takeWhileC1 isDigit
          pure (addDigits n1 frac, negate $ fromIntegral $ BSL.length frac)
      takeIf (\e -> e == 'e' || e == 'E') >>= \case
        Nothing -> pure (n2, e1)
        Just _  -> anyChar >>= \case
          '+' -> do
            e2 <- addDigits 0 <$> takeWhileC1 isDigit
            pure (n2, e1 + e2)
          '-' -> do
            e2 <- addDigits 0 <$> takeWhileC1 isDigit
            pure (n2, e1 - e2)
          s | isDigit c -> do
            e2 <- addDigits (ord s - ord '0') <$> takeWhileC isDigit
            pure (n2, e1 + e2)
            | otherwise -> fail $ "Unexpected " ++ show s

    addDigits :: Integral a => a -> BSL.ByteString -> a
    addDigits = BSL.foldl' addDigit

    addDigit :: Integral a => a -> Char -> a
    addDigit n d = n * 10 + fromIntegral (ord d - ord '0')

parseString :: ValueParser s BSL.ByteString
parseString = ValueParser $ \case
  '"' -> Just stringParser'
  _   -> Nothing

parseArray :: ArrayParser s a a -> ValueParser s a
parseArray p = ValueParser $ \case
  '[' -> Just $ do
    skipWhile jsonWhitespace
    runArrayParser p (\x _ -> x <$ char ']') 0
  _   -> Nothing

parseObject :: ObjectParser s a -> ValueParser s a
parseObject (ObjectParser obj) = ValueParser $ \case
  '{' -> Just $ do
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

  _   -> Nothing

-- |Parses any valid json value and ignores it
parseIgnore :: ValueParser s ()
parseIgnore = parseNull
           <> parseFalse
           <> parseTrue
           <> (() <$ parseNumber)
           <> (() <$ parseString)
           <> parseArray ignoreArray
           <> parseObject ignoreAnyField

valueParser :: ValueParser s a -> Parser s a
valueParser parser = do
  c <- anyChar
  valueParser' c parser

valueParser' :: Char -> ValueParser s a -> Parser s a
valueParser' c (ValueParser vp) = case vp c of
  Nothing -> fail $ "Unexpected " ++ show c
  Just p  -> p <* skipWhile jsonWhitespace

stringParser' :: Parser s BSL.ByteString
stringParser' = fmap BSL.concat parser
  where
    parser :: Parser s [BSL.ByteString]
    parser = do
      nonEscaped <- takeWhileC $ not . mustEscape
      fmap (nonEscaped :) $ anyChar >>= \case
        '"'  -> pure []
        '\\' -> anyChar >>= \case
          '"'  -> ("\"" :) <$> parser
          '\\' -> ("\\" :) <$> parser
          '/'  -> ("/"  :) <$> parser
          'b'  -> ("\b" :) <$> parser
          'f'  -> ("\f" :) <$> parser
          'n'  -> ("\n" :) <$> parser
          'r'  -> ("\r" :) <$> parser
          't'  -> ("\t" :) <$> parser
          'u'  -> do
            cp <- unicodeSymbol
            encoded <-
              if | cp < 128  -> pure [chr cp]
                 | cp < 2048 -> do
                   let (c1, c2) = cp `divMod` 64
                   pure $ map chr [c1 + 192, c2 + 128]
                 | 0xD800 <= cp && cp < 0xDC00 -> do
                   string "\\u"
                   cp2 <- unicodeSymbol
                   when (cp < 0xDC00 || cp > 0xDFFF)
                     $ fail "Expecting low surrogate"
                   let realCp = (cp `mod` 1024) * 1024 + cp2 `mod` 1024
                       (c123, c4) = realCp `divMod` 64
                       (c12, c3)  = c123 `divMod` 64
                       (c1, c2)   = c12 `divMod` 64
                   pure $ map chr [c1 + 240, c2 + 128, c3 + 128, c4 + 128]
                 | otherwise -> do
                   let (c12, c3) = cp  `divMod` 64
                       (c1, c2)  = c12 `divMod` 64
                   pure $ map chr [c1 + 224, c2 + 128, c3 + 128]
            (BSL.pack encoded :) <$> parser
          c    -> fail $ "Unexpected " ++ show c
        c    -> fail $ "Unexpected " ++ show c

    mustEscape c = c == '"' || c == '\\' || ord c < 32

    hexDigit = anyChar >>= \case
      c | '0' <= c && c <= '9' -> pure $ ord c - ord '0'
        | 'a' <= c && c <= 'f' -> pure $ ord c - ord 'a' + 10
        | 'A' <= c && c <= 'F' -> pure $ ord c - ord 'A' + 10
        | otherwise            -> fail $ show c ++ " is not a valid hex digit"

    unicodeSymbol = do
      d1 <- hexDigit
      d2 <- hexDigit
      d3 <- hexDigit
      d4 <- hexDigit
      pure $ ((d1 * 16 + d2) * 16 + d3) * 16 + d4

newtype ArrayParser s r a = ArrayParser { runArrayParser :: (a -> Int -> Parser s r) -> Int -> Parser s r }

instance Functor (ArrayParser s r) where
  fmap f (ArrayParser p) = ArrayParser $ \cont -> p $ cont . f

instance Applicative (ArrayParser s r) where
  pure x = ArrayParser ($ x)
  ArrayParser mf <*> ArrayParser mx = ArrayParser $ \cont ->
    mf $ \cf -> mx $ cont . cf

instance Monad (ArrayParser r a) where
  ArrayParser mx >>= f = ArrayParser $ \cont ->
    mx $ \cx -> runArrayParser (f cx) cont

someElement :: Parser s r -> ValueParser s a -> ArrayParser s r a
someElement end single = ArrayParser $ \cont n ->
  anyChar >>= \case
    ']' -> end
    c | n == 0 -> do
      !x <- atArrayIndex 0 $ valueParser' c single
      cont x 1
    ',' -> do
      skipWhile jsonWhitespace
      !x <- atArrayIndex n $ valueParser single
      cont x $ n + 1
    _   -> fail "Expected ',' or ']'"
{-# INLINE someElement #-}

requiredElement :: ValueParser s a -> ArrayParser s r a
requiredElement = someElement $ fail "Expected required element"
{-# INLINE requiredElement #-}

optionalElement :: r -> ValueParser s a -> ArrayParser s r a
optionalElement = someElement . pure
{-# INLINE optionalElement #-}

ignoreArray :: ArrayParser s () ()
ignoreArray = optionalElement () parseIgnore *> ignoreArray

arrayOf :: ValueParser s a -> ArrayParser s [a] [a]
arrayOf single = go []
  where go xs = optionalElement (reverse xs) single >>= \x -> go (x:xs)

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
