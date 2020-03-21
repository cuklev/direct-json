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
      let parser = () <$ jsonObject jsonFieldInvalidUnknown
      runParser parser "{}" `shouldBe` Right ()

    it "object with a field" $ do
      let parser = do
            x :+ _ <- jsonObject $ "id" -: jsonFieldInvalidUnknown
            pure x
      runParser parser "{\"id\":4}" `shouldBe` Right (4 :: Int)

    it "object with two fields" $ do
      let parser = do
            age :+ name :+ _ <- jsonObject $ "age" -: "name" -: jsonFieldInvalidUnknown
            pure (age, name)
      runParser parser "{\"age\":4,\"name\":\"John\"}" `shouldBe` Right (4 :: Int, "John" :: BSL.ByteString)

    it "object with two fields (different order)" $ do
      let parser = do
            age :+ name :+ _ <- jsonObject $ "age" -: "name" -: jsonFieldInvalidUnknown
            pure (age, name)
      runParser parser "{\"name\":\"John\",\"age\":4}" `shouldBe` Right (4 :: Int, "John" :: BSL.ByteString)

    it "homegenous object" $ do
      let parser = do
            m :+ _ <- jsonObject jsonFieldCaptureUnknown
            pure $ sort m
      runParser parser "{\"age\":4,\"id\":42,\"count\":5}" `shouldBe` Right (sort [("age", 4), ("id", 42), ("count", 5 :: Int)])
