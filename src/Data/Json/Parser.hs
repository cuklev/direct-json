{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Data.Json.Parser
  ( Parser
  , atKey
  , atIndex
  , takeWhileC
  , takeWhileC1
  , anyChar
  , char
  , string
  , selectOnFirstChar
  , runParser
  ) where

import Prelude hiding (fail)
import Control.Applicative (Alternative (..))
import Control.Monad.Fail (MonadFail (..))
import Data.Bifunctor (first, bimap)
import qualified Data.ByteString.Lazy.Char8 as BSL

data Nested
  = NestedField                !BSL.ByteString
  | NestedIndex {-# UNPACK #-} !Int

data ParseError = ParseError [Nested] String

newtype Parser a = Parser ([Nested] -> BSL.ByteString -> Either ParseError (a, BSL.ByteString))

atKey :: BSL.ByteString -> Parser a -> Parser a
atKey key (Parser p) = Parser $ \nested -> p (NestedField key : nested)
{-# INLINE atKey #-}

atIndex :: Int -> Parser a -> Parser a
atIndex index (Parser p) = Parser $ \nested -> p (NestedIndex index : nested)
{-# INLINE atIndex #-}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \nested -> fmap (first f) . p nested
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure x = Parser $ \_ -> Right . (x,)
  {-# INLINE pure #-}

  Parser p1 <*> Parser p2 = Parser $ \nested input -> do
    (f, next) <- p1 nested input
    first f <$> p2 nested next
  {-# INLINE (<*>) #-}

instance Alternative Parser where
  empty = fail "empty"
  {-# INLINE empty #-}

  Parser p1 <|> Parser p2 = Parser $ \nested input ->
    case p1 nested input of
      Left _       -> p2 nested input
      Right result -> Right result
  {-# INLINE (<|>) #-}

instance Monad Parser where
  Parser p >>= f = Parser $ \nested input -> do
    (x, next) <- p nested input
    let Parser result = f x
    result nested next
  {-# INLINE (>>=) #-}

instance MonadFail Parser where
  fail err = Parser $ \nested _ -> Left $ ParseError nested err
  {-# INLINE fail #-}

takeWhileC :: (Char -> Bool) -> Parser BSL.ByteString
takeWhileC f = Parser $ \_ -> Right . BSL.span f
{-# INLINE takeWhileC #-}

takeWhileC1 :: (Char -> Bool) -> Parser BSL.ByteString
takeWhileC1 f = Parser $ \nested input ->
  let (res, rest) = BSL.span f input
  in if BSL.null res
        then Left $ ParseError nested "Got none (takeWhileC1)"
        else Right (res, rest)
{-# INLINE takeWhileC1 #-}

anyChar :: Parser Char
anyChar = Parser $ \nested -> maybe (Left $ ParseError nested "Expected any char, got end of input") Right . BSL.uncons
{-# INLINE anyChar #-}

char :: Char -> Parser ()
char c = do
  c' <- anyChar <|> fail ("Expected " ++ show c ++ ", but got end of input")
  if c == c' then pure ()
             else fail ("Expected " ++ show c)
{-# INLINE char #-}

string :: BSL.ByteString -> Parser ()
string prefix = Parser $ \nested input ->
  case BSL.stripPrefix prefix input of
    Nothing   -> Left $ ParseError nested $ "Expected " ++ show prefix
    Just next -> Right ((), next)
{-# INLINE string #-}

selectOnFirstChar :: Parser a -> [(Char, Parser a)] -> Parser a
selectOnFirstChar (Parser fallback) parsers =
  Parser $ \nested input -> do
    let Parser get = anyChar
    (c, next) <- get nested input
    case lookup c parsers of
      Nothing -> fallback nested input
      Just (Parser p) -> p nested next
{-# INLINE selectOnFirstChar #-}


runParser :: Parser a -> BSL.ByteString -> Either String a
runParser (Parser p) = bimap showError fst . p []
  where
    showError (ParseError nested str) = "root" ++ concatMap showWrapped (reverse nested) ++ ": " ++ str
    showWrapped nested = "[" ++ showNested nested ++ "]"
    showNested = \case
      NestedField key -> show key
      NestedIndex ind -> show ind
{-# INLINE runParser #-}
