{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Data.JsonSpec
  ( spec
  ) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Vector as V
import Data.Foldable (traverse_)
import Data.Json
import Data.Json.Parser
import Data.List (sort)
import Test.Hspec

spec :: Spec
spec = do
  describe "parsing positive integers" $
    let test n = it (show n) $ parseJson (BSL.pack $ show n) `shouldBe` Right n
    in traverse_ test [1, 5, 42, 123439 :: Int]

  describe "parsing simple strings" $
    let test s = it (show s) $ parseJson ("\"" <> s <> "\"") `shouldBe` Right s
    in traverse_ test ["", "hello", "x", "ti80"]

  describe "parsing null" $
    it "null" $ parseJson "null" `shouldBe` Right ()

  describe "parsing bools" $ do
    it "false" $ parseJson "false" `shouldBe` Right False
    it "true"  $ parseJson "true"  `shouldBe` Right True

  describe "parsing lists" $
    let test s = do
          it ("list " ++ show s) $ parseJson (BSL.pack $ show s) `shouldBe` Right s
          it ("vector " ++ show s) $ parseJson (BSL.pack $ show s) `shouldBe` Right (V.fromList s)
    in traverse_ test [[], [5], [1, 2], [1, 2, 3 :: Int]]

  describe "parsing objects" $ do
    it "empty" $ do
      let res = runParser (jsonObject jsonFieldInvalidUnknown) "{}"
      case res of
        Right _  -> pure () :: IO ()
        Left err -> fail err

    it "object with a field" $ do
      let res = runParser (jsonObject $ "id" -: jsonFieldInvalidUnknown) "{\"id\":4}"
      case res of
        Right (x :+ _)
                 -> x `shouldBe` (4 :: Int)
        Left err -> fail err

    it "object with two fields" $ do
      let res = runParser (jsonObject $ "age" -: "name" -: jsonFieldInvalidUnknown) "{\"age\":4,\"name\":\"John\"}"
      case res of
        Right (age :+ name :+ _)
                 -> (age, name) `shouldBe` (4 :: Int, "John" :: BSL.ByteString)
        Left err -> fail err

    it "object with two fields" $ do
      let res = runParser (jsonObject $ "name" -: "age" -: jsonFieldInvalidUnknown) "{\"age\":4,\"name\":\"John\"}"
      case res of
        Right (name :+ age :+ _)
                 -> (age, name) `shouldBe` ((4, "John") :: (Int, BSL.ByteString))
        Left err -> fail err

    it "homegenous object" $ do
      let res = runParser (jsonObject jsonFieldCaptureUnknown) "{\"age\":4,\"id\":42,\"count\":5}"
      case res of
        Right (m :+ _)
                 -> sort m `shouldBe` sort [("age", 4), ("id", 42), ("count", 5 :: Int)]
        Left err -> fail err
