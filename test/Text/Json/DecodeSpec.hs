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
    it "success" $
      decode (parseNull ()) "null" `shouldBe` Right ()
    it "fail" $
      decode (parseNull ()) "false" `shouldSatisfy` isLeft
    it "fail" $
      decode (parseNull ()) "nulla" `shouldSatisfy` isLeft

  describe "bool parsing" $ do
    let boolParser = parseFalse False <> parseTrue True
    it "false" $
      decode boolParser "false" `shouldBe` Right False
    it "true" $
      decode boolParser "true" `shouldBe` Right True
    it "null" $
      decode boolParser "null" `shouldSatisfy` isLeft

  describe "string parsing" $ do
    it "empty string" $
      decode (parseString id) "\"\"" `shouldBe` Right ""
    it "some simple string" $
      decode (parseString id) "\"asdf\"" `shouldBe` Right "asdf"
    it "string with escaping" $
      decode (parseString id) "\"asdf\\nasdf\"" `shouldBe` Right "asdf\nasdf"
    it "missing openning quote" $
      decode (parseString id) "asdf\"" `shouldSatisfy` isLeft
    it "missing closing quote" $
      decode (parseString id) "\"asdf" `shouldSatisfy` isLeft

  describe "array parsing" $ do
    let nullArrayParser = parseArray (parseNull ()) (\(_, xs) -> reverse xs)
    it "empty array" $
      decode nullArrayParser "[]" `shouldBe` Right []
    it "single value array" $
      decode nullArrayParser "[null]" `shouldBe` Right [()]
    it "longer array" $
      decode nullArrayParser "[null,null,null]" `shouldBe` Right [(),(),()]
