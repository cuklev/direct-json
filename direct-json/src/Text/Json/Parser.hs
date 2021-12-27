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
  , takeWhileC1
  , skipWhile
  , takeIf
  ) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Bifunctor (first, second)
import Data.STRef
import Control.Monad.ST

data Location
  = LocationArrayIndex !Int
  | LocationObjectKey !BSL.ByteString

newtype Parser s a = Parser (STRef s BSL.ByteString -> ST s (Either (String, [Location]) a))

instance Functor (Parser s) where
  fmap f (Parser parser) = Parser $ \ref -> second f <$> parser ref

instance Applicative (Parser s) where
  pure x = Parser $ \_ -> pure $ Right x
  Parser p1 <*> Parser p2 = Parser $ \ref ->
    p1 ref >>= \case
      Left e -> pure $ Left e
      Right f -> p2 ref >>= \case
        Left e -> pure $ Left e
        Right x -> pure $ Right $ f x

instance Monad (Parser s) where
  Parser p >>= f = Parser $ \ref -> do
    p ref >>= \case
      Left e -> pure $ Left e
      Right x -> do
        let Parser p2 = f x
        p2 ref

instance MonadFail (Parser s) where
  fail msg = Parser $ \_ -> pure $ Left (msg, [])

runParser :: Parser s a -> BSL.ByteString -> ST s (Either (String, [Location]) a)
runParser json input = parser =<< newSTRef input
  where Parser parser = json <* eof

eof :: Parser s ()
eof = Parser $ \ref -> do
  input <- readSTRef ref
  pure $ if BSL.null input
    then Right ()
    else Left ("Expected end of input", [])

liftST :: ST s a -> Parser s a
liftST action = Parser $ \_ -> fmap Right action

atLocation :: Location -> Parser s a -> Parser s a
atLocation sub (Parser parser) = Parser $ fmap (first (second (sub :))) . parser

atArrayIndex :: Int -> Parser s a -> Parser s a
atArrayIndex = atLocation . LocationArrayIndex

atObjectKey :: BSL.ByteString -> Parser s a -> Parser s a
atObjectKey = atLocation . LocationObjectKey

showError :: (String, [Location]) -> String
showError (msg, locs) = "$" ++ concatMap showLocation locs ++ ": " ++ msg
  where
    showLocation = \case
      LocationArrayIndex index -> "[" ++ show index ++ "]"
      LocationObjectKey  key   -> "[" ++ show key   ++ "]"

anyChar :: Parser s Char
anyChar = Parser $ \ref -> do
  input <- readSTRef ref
  case BSL.uncons input of
    Nothing -> pure $ Left ("Unexpected end of input", [])
    Just (x, xs) -> do
      writeSTRef ref xs
      pure $ Right x

char :: Char -> Parser s ()
char c = Parser $ \ref -> do
  input <- readSTRef ref
  case BSL.uncons input of
    Nothing -> pure $ Left ("Unexpected end of input", [])
    Just (x, xs)
      | c /= x -> pure $ Left ("Unexpected " ++ show x, [])
      | otherwise -> do
          writeSTRef ref xs
          pure $ Right ()

satisfy :: (Char -> Bool) -> Parser s Char
satisfy f = Parser $ \ref -> do
  input <- readSTRef ref
  case BSL.uncons input of
    Nothing -> pure $ Left ("Unexpected end of input", [])
    Just (x, xs)
      | f x -> do
          writeSTRef ref xs
          pure $ Right x
      | otherwise -> pure $ Left ("Unexpected " ++ show x, [])

string :: BSL.ByteString -> Parser s ()
string prefix = Parser $ \ref -> do
  input <- readSTRef ref
  case BSL.stripPrefix prefix input of
    Nothing -> pure $ Left ("no match", [])
    Just suffix -> do
      writeSTRef ref suffix
      pure $ Right ()

takeWhileC :: (Char -> Bool) -> Parser s BSL.ByteString
takeWhileC f = Parser $ \ref -> do
  input <- readSTRef ref
  let (prefix, suffix) = BSL.span f input
  writeSTRef ref suffix
  pure $ Right prefix

takeWhileC1 :: (Char -> Bool) -> Parser s BSL.ByteString
takeWhileC1 f = Parser $ \ref -> do
  input <- readSTRef ref
  let (prefix, suffix) = BSL.span f input
  if BSL.null prefix
    then pure $ Left ("empty", [])
    else do
      writeSTRef ref suffix
      pure $ Right prefix

skipWhile :: (Char -> Bool) -> Parser s ()
skipWhile f = Parser $ \ref -> do
  modifySTRef' ref $ BSL.dropWhile f
  pure $ Right ()

takeIf :: (Char -> Bool) -> Parser s (Maybe Char)
takeIf f = Parser $ \ref -> do
  input <- readSTRef ref
  case BSL.uncons input of
    Just (x, xs)
      | f x -> do
        writeSTRef ref xs
        pure $ Right $ Just x
    _ -> pure $ Right Nothing
