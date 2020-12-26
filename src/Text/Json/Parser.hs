{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Text.Json.Parser
  ( Parser
  , runParser
  , liftST
  , atArrayIndex
  , atObjectKey
  , showError
  , anyChar
  , char
  , satisfy
  , string
  , takeWhileC
  , skipWhile
  ) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Bifunctor (second)
import Data.STRef
import Control.Monad.ST

data Location
  = LocationArrayIndex !Int
  | LocationObjectKey !BSL.ByteString

newtype Parser s a = Parser ([Location] -> STRef s BSL.ByteString -> ST s (Either (String, [Location]) a))

instance Functor (Parser s) where
  fmap f (Parser parser) = Parser $ \loc ref -> second f <$> parser loc ref

instance Applicative (Parser s) where
  pure x = Parser $ \_ _ -> pure $ Right x
  Parser p1 <*> Parser p2 = Parser $ \loc ref ->
    p1 loc ref >>= \case
      Left e -> pure $ Left e
      Right f -> p2 loc ref >>= \case
        Left e -> pure $ Left e
        Right x -> pure $ Right $ f x

instance Monad (Parser s) where
  Parser p >>= f = Parser $ \loc ref -> do
    p loc ref >>= \case
      Left e -> pure $ Left e
      Right x -> do
        let Parser p2 = f x
        p2 loc ref

instance MonadFail (Parser s) where
  fail msg = Parser $ \loc _ -> pure $ Left (msg, loc)

runParser :: Parser s a -> BSL.ByteString -> ST s (Either (String, [Location]) a)
runParser json input = parser [] =<< newSTRef input
  where Parser parser = json <* eof

eof :: Parser s ()
eof = Parser $ \loc ref -> do
  input <- readSTRef ref
  pure $ if BSL.null input
    then Right ()
    else Left ("Expected end of input", loc)

liftST :: ST s a -> Parser s a
liftST action = Parser $ \_ _ -> fmap Right action

atLocation :: Location -> Parser s a -> Parser s a
atLocation sub (Parser parser) = Parser $ parser . (sub:)

atArrayIndex :: Int -> Parser s a -> Parser s a
atArrayIndex = atLocation . LocationArrayIndex

atObjectKey :: BSL.ByteString -> Parser s a -> Parser s a
atObjectKey = atLocation . LocationObjectKey

showError :: (String, [Location]) -> String
showError (msg, locs) = "$" ++ concatMap showLocation (reverse locs) ++ ": " ++ msg
  where
    showLocation = \case
      LocationArrayIndex index -> "[" ++ show index ++ "]"
      LocationObjectKey  key   -> "[" ++ show key   ++ "]"

anyChar :: Parser s Char
anyChar = Parser $ \loc ref -> do
  input <- readSTRef ref
  case BSL.uncons input of
    Nothing -> pure $ Left ("Unexpected end of input", loc)
    Just (x, xs) -> do
      writeSTRef ref xs
      pure $ Right x

char :: Char -> Parser s ()
char c = Parser $ \loc ref -> do
  input <- readSTRef ref
  case BSL.uncons input of
    Nothing -> pure $ Left ("Unexpected end of input", loc)
    Just (x, xs)
      | c /= x -> pure $ Left ("Unexpected " ++ show x, loc)
      | otherwise -> do
          writeSTRef ref xs
          pure $ Right ()

satisfy :: (Char -> Bool) -> Parser s Char
satisfy f = Parser $ \loc ref -> do
  input <- readSTRef ref
  case BSL.uncons input of
    Nothing -> pure $ Left ("Unexpected end of input", loc)
    Just (x, xs)
      | f x -> pure $ Left ("Unexpected " ++ show x, loc)
      | otherwise -> do
          writeSTRef ref xs
          pure $ Right x

string :: BSL.ByteString -> Parser s ()
string prefix = Parser $ \loc ref -> do
  input <- readSTRef ref
  case BSL.stripPrefix prefix input of
    Nothing -> pure $ Left ("no match", loc)
    Just suffix -> do
      writeSTRef ref suffix
      pure $ Right ()

takeWhileC :: (Char -> Bool) -> Parser s BSL.ByteString
takeWhileC f = Parser $ \_ ref -> do
  input <- readSTRef ref
  let (prefix, suffix) = BSL.span f input
  writeSTRef ref suffix
  pure $ Right prefix

skipWhile :: (Char -> Bool) -> Parser s ()
skipWhile f = Parser $ \_ ref -> do
  modifySTRef' ref $ BSL.dropWhile f
  pure $ Right ()
