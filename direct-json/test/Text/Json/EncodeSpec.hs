{-# LANGUAGE OverloadedStrings #-}
module Text.Json.EncodeSpec
  ( spec
  ) where

import Data.Scientific (normalize)
import Test.Hspec
import Text.Json.Encode

spec :: Spec
spec = do
  describe "contants" $ do
    it "null" $
      encode encodeNull `shouldBe` "null"
    it "false" $
      encode encodeFalse `shouldBe` "false"
    it "true" $
      encode encodeTrue `shouldBe` "true"

  describe "numbers" $ do
    it "0" $
      encode (encodeNumber 0) `shouldBe` "0"
    it "4" $
      encode (encodeNumber 4) `shouldBe` "4"
    it "7324" $
      encode (encodeNumber 7324) `shouldBe` "7324"
    it "-13" $
      encode (encodeNumber (-13)) `shouldBe` "-13"
    it "4.2" $
      encode (encodeNumber 4.2) `shouldBe` "4.2"
    it "4.2e1" $
      encode (encodeNumber 4.2e1) `shouldBe` "42"
    it "5e100" $
      encode (encodeNumber $ normalize 5e100) `shouldBe` "5.0e100"
    it "5e-100" $
      encode (encodeNumber 5e-100) `shouldBe` "5.0e-100"

  describe "strings" $ do
    it "empty string" $
      encode (encodeString "") `shouldBe` "\"\""
    it "singleton string" $
      encode (encodeString "@") `shouldBe` "\"@\""
    it "some string" $
      encode (encodeString "hello") `shouldBe` "\"hello\""
    it "escaping \\\"" $
      encode (encodeString "one\"two") `shouldBe` "\"one\\\"two\""
    it "escaping \\\\" $
      encode (encodeString "one\\two") `shouldBe` "\"one\\\\two\""
    it "escaping \\n" $
      encode (encodeString "one\ntwo") `shouldBe` "\"one\\ntwo\""
    it "escaping \\0" $
      encode (encodeString "one\0two") `shouldBe` "\"one\\u0000two\""

  describe "arrays" $ do
    it "empty" $
      encode (encodeArray []) `shouldBe` "[]"
    it "with values" $
      encode (encodeArray [encodeNull, encodeFalse, encodeTrue]) `shouldBe` "[null,false,true]"
    it "nested" $ do
      let inner = encodeArray [encodeNumber 1, encodeNumber 2, encodeNumber 3]
          outer = encodeArray [inner, inner, inner]
      encode outer `shouldBe` "[[1,2,3],[1,2,3],[1,2,3]]"

  describe "objects" $ do
    it "empty" $
      encode (encodeObject []) `shouldBe` "{}"
    it "with values" $
      encode (encodeObject [("a", encodeNull), ("b", encodeFalse), ("c", encodeTrue)]) `shouldBe` "{\"a\":null,\"b\":false,\"c\":true}"
    it "nested" $ do
      let inner = encodeObject [("one", encodeNumber 1), ("two", encodeNumber 2), ("three", encodeNumber 3)]
          outer = encodeObject [("first", inner), ("second", inner), ("third", inner)]
      encode outer `shouldBe` "{\"first\":{\"one\":1,\"two\":2,\"three\":3},\"second\":{\"one\":1,\"two\":2,\"three\":3},\"third\":{\"one\":1,\"two\":2,\"three\":3}}"
