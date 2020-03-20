{-# LANGUAGE TupleSections #-}
module Data.Json.Parser
  ( Parser
  , takeWhileC
  , takeWhileC1
  , anyChar
  , char
  , string
  , runParser
  ) where

import Prelude hiding (fail)
import Control.Applicative (Alternative (..))
import Control.Monad.Fail (MonadFail (..))
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy.Char8 as BSL

newtype Parser a = Parser (BSL.ByteString -> Either String (a, BSL.ByteString))

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (first f) . p

instance Applicative Parser where
  pure x = Parser $ Right . (x,)
  Parser p1 <*> Parser p2 = Parser $ \input -> do
    (f, next) <- p1 input
    first f <$> p2 next

instance Alternative Parser where
  empty = Parser $ const $ Left "empty"
  Parser p1 <|> Parser p2 = Parser $ \input ->
    case p1 input of
      Left _       -> p2 input
      Right result -> Right result

instance Monad Parser where
  Parser p >>= f = Parser $ \input -> do
    (x, next) <- p input
    let Parser result = f x
    result next

instance MonadFail Parser where
  fail err = Parser $ \_ -> Left err

takeWhileC :: (Char -> Bool) -> Parser BSL.ByteString
takeWhileC f = Parser $ Right . BSL.span f

takeWhileC1 :: (Char -> Bool) -> Parser BSL.ByteString
takeWhileC1 f = Parser $ \input ->
  let (res, rest) = BSL.span f input
  in if BSL.null res
        then Left "Got none (takeWhileC1)"
        else Right (res, rest)

anyChar :: Parser Char
anyChar = Parser $ maybe (Left "Expected any char, got end of input") Right . BSL.uncons

char :: Char -> Parser ()
char c = do
  c' <- anyChar <|> fail ("Expected " ++ show c ++ ", but got end of input")
  if c == c' then pure ()
             else fail ("Expected " ++ show c)

string :: BSL.ByteString -> Parser ()
string prefix = Parser $ \input ->
  case BSL.stripPrefix prefix input of
    Nothing   -> Left $ "Expected " ++ show prefix
    Just next -> Right ((), next)

runParser :: Parser a -> BSL.ByteString -> Either String a
runParser (Parser p) = fmap fst . p
