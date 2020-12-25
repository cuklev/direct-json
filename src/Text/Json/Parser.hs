{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Text.Json.Parser
  ( Parser
  , runParser
  , liftST
  , anyChar
  , char
  , string
  , takeWhileC
  ) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Bifunctor (second)
import Data.STRef
import Control.Monad.ST

data Location = Location

newtype Parser a = Parser (forall s. Location -> STRef s BSL.ByteString -> ST s (Either String a))

instance Functor Parser where
  fmap f (Parser parser) = Parser $ \loc ref -> second f <$> parser loc ref

instance Applicative Parser where
  pure x = Parser $ \_ _ -> pure $ Right x
  Parser p1 <*> Parser p2 = Parser $ \loc ref ->
    p1 loc ref >>= \case
      Left e -> pure $ Left e
      Right f -> p2 loc ref >>= \case
        Left e -> pure $ Left e
        Right x -> pure $ Right $ f x

instance Monad Parser where
  Parser p >>= f = Parser $ \loc ref -> do
    p loc ref >>= \case
      Left e -> pure $ Left e
      Right x -> do
        let Parser p2 = f x
        p2 loc ref

instance MonadFail Parser where
  fail msg = Parser $ \_ _ -> pure $ Left msg

runParser :: Parser a -> BSL.ByteString -> ST s (Either String a)
runParser (Parser p) input = p Location =<< newSTRef input

liftST :: (forall s. ST s a) -> Parser a
liftST action = Parser $ \_ _ -> fmap Right action

anyChar :: Parser Char
anyChar = Parser $ \_ ref -> do
  input <- readSTRef ref
  case BSL.uncons input of
    Nothing -> pure $ Left "Unexpected end of input"
    Just (x, xs) -> do
      writeSTRef ref xs
      pure $ Right x

char :: Char -> Parser ()
char c = Parser $ \_ ref -> do
  input <- readSTRef ref
  case BSL.uncons input of
    Nothing -> pure $ Left "Unexpected end of input"
    Just (x, xs)
      | c /= x -> pure $ Left $ "Unexpected " ++ show x
      | otherwise -> do
          writeSTRef ref xs
          pure $ Right ()

string :: BSL.ByteString -> Parser ()
string prefix = Parser $ \_ ref -> do
  input <- readSTRef ref
  case BSL.stripPrefix prefix input of
    Nothing -> pure $ Left "no match"
    Just suffix -> do
      writeSTRef ref suffix
      pure $ Right ()

takeWhileC :: (Char -> Bool) -> Parser BSL.ByteString
takeWhileC f = Parser $ \_ ref -> do
  input <- readSTRef ref
  let (prefix, suffix) = BSL.span f input
  writeSTRef ref suffix
  pure $ Right prefix
