{-# LANGUAGE OverloadedStrings #-}
module Text.Json.DecodeSpec
  ( spec
  ) where

import Data.Either
import Test.Hspec
import Text.Json.Decode

spec :: Spec
spec = do
  describe "parsing null" $ do
    let nullParser = NullParser () Empty
    it "success" $
      decode nullParser "null" `shouldBe` Right ()
    it "fail" $
      decode nullParser "false" `shouldSatisfy` isLeft
    it "fail" $
      decode nullParser "nulla" `shouldSatisfy` isLeft

  describe "bool parsing" $ do
    let boolParser = FalseParser False $ TrueParser True Empty
    it "false" $
      decode boolParser "false" `shouldBe` Right False
    it "true" $
      decode boolParser "true" `shouldBe` Right True
    it "null" $
      decode boolParser "null" `shouldSatisfy` isLeft

  describe "string parsing" $ do
    let stringParser = StringParser id Empty
    it "empty string" $
      decode stringParser "\"\"" `shouldBe` Right ""
    it "some simple string" $
      decode stringParser "\"asdf\"" `shouldBe` Right "asdf"
    it "string with escaping" $
      decode stringParser "\"asdf\\nasdf\"" `shouldBe` Right "asdf\nasdf"
    it "missing openning quote" $
      decode stringParser "asdf\"" `shouldSatisfy` isLeft
    it "missing closing quote" $
      decode stringParser "\"asdf" `shouldSatisfy` isLeft
