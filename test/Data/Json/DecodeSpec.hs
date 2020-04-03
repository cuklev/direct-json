{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Data.Json.DecodeSpec
  ( spec
  ) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Vector as V
import Data.Foldable (traverse_)
import Data.Json.Decode
import Data.Json.Parser
import Data.List (sort)
import Test.Hspec

spec :: Spec
spec = describe "Parsing" $ do
  describe "parsing positive integers" $
    let test n = it (show n) $ decode (BSL.pack $ show n) `shouldBe` Right n
    in traverse_ test [1, 5, 42, -17, 123439 :: Int]

  describe "parsing simple strings" $
    let test s = it (show s) $ decode ("\"" <> s <> "\"") `shouldBe` Right s
    in traverse_ test ["", "hello", "x", "ti80"]

  describe "parsing null" $
    it "null" $ decode "null" `shouldBe` Right ()

  describe "parsing bools" $ do
    it "false" $ decode "false" `shouldBe` Right False
    it "true"  $ decode "true"  `shouldBe` Right True

  describe "parsing lists" $
    let test s = do
          it ("list " ++ show s) $ decode (BSL.pack $ show s) `shouldBe` Right s
          it ("vector " ++ show s) $ decode (BSL.pack $ show s) `shouldBe` Right (V.fromList s)
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

  describe "Json type" $
    it "Json type" $ do
      decode "{\"empty object\":{},\"empty list\":[],\"values\":[42,false,true,null,\"somevalue\"]}" `shouldBe`
        Right
          (JsonObject $ V.fromList
            [ ("values", JsonList $ V.fromList
                [ JsonNumber 42
                , JsonFalse
                , JsonTrue
                , JsonNull
                , JsonString "somevalue"
                ])
            , ("empty list", JsonList V.empty)
            , ("empty object", JsonObject V.empty)
            ])
