{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
module Text.Json.DecodeSpec
  ( spec
  ) where

import Data.Either
import Test.Hspec
import Text.Json.Decode

spec :: Spec
spec = do
  describe "parsing null" $ do
    it "null constant" $
      decode parseNull "null" `shouldBe` Right ()
    it "wrong constant" $
      decode parseNull "everything" `shouldSatisfy` isLeft
    it "excess input" $
      decode parseNull "nulla" `shouldSatisfy` isLeft

  describe "bool parsing" $ do
    let boolParser = (False <$ parseFalse) <> (True <$ parseTrue)
    it "false" $
      decode boolParser "false" `shouldBe` Right False
    it "true" $
      decode boolParser "true" `shouldBe` Right True
    it "wrong constant" $
      decode boolParser "null" `shouldSatisfy` isLeft

  describe "number parsing" $ do
    it "0" $
      decode parseNumber "0" `shouldBe` Right 0
    it "4" $
      decode parseNumber "4" `shouldBe` Right 4
    it "1234" $
      decode parseNumber "1234" `shouldBe` Right 1234
    it "-4" $
      decode parseNumber "-4" `shouldBe` Right (-4)
    it "1234" $
      decode parseNumber "-1234" `shouldBe` Right (-1234)

    it "01 should be invalid" $
      decode parseNumber "01" `shouldSatisfy` isLeft

    it "4.2" $
      decode parseNumber "4.2" `shouldBe` Right 4.2
    it "4e1" $
      decode parseNumber "4e1" `shouldBe` Right 40
    it "4e+1" $
      decode parseNumber "4e+1" `shouldBe` Right 40
    it "4e-1" $
      decode parseNumber "4e-1" `shouldBe` Right 0.4
    it "1.23e-5" $
      decode parseNumber "1.23e-5" `shouldBe` Right 1.23e-5
    it "-1.23e-5" $
      decode parseNumber "-1.23e-5" `shouldBe` Right (-1.23e-5)

    it "floating number between 0 and 1" $
      decode parseNumber "0.3" `shouldBe` Right 0.3
    it "floating number between 0 and -1" $
      decode parseNumber "-0.3" `shouldBe` Right (-0.3)

    it "-01 should be invalid" $
      decode parseNumber "-01" `shouldSatisfy` isLeft

  describe "string parsing" $ do
    it "empty string" $
      decode parseString "\"\"" `shouldBe` Right ""
    it "some simple string" $
      decode parseString "\"asdf\"" `shouldBe` Right "asdf"
    it "missing openning quote" $
      decode parseString "asdf\"" `shouldSatisfy` isLeft
    it "missing closing quote" $
      decode parseString "\"asdf" `shouldSatisfy` isLeft

    describe "escaped symbols" $ do
      it "string with \\\"" $
        decode parseString "\"asdf\\\"asdf\"" `shouldBe` Right "asdf\"asdf"
      it "string with \\\\" $
        decode parseString "\"asdf\\\\asdf\"" `shouldBe` Right "asdf\\asdf"
      it "string with \\/" $
        decode parseString "\"asdf\\/asdf\"" `shouldBe` Right "asdf/asdf"
      it "string with \\b" $
        decode parseString "\"asdf\\basdf\"" `shouldBe` Right "asdf\basdf"
      it "string with \\f" $
        decode parseString "\"asdf\\fasdf\"" `shouldBe` Right "asdf\fasdf"
      it "string with \\n" $
        decode parseString "\"asdf\\nasdf\"" `shouldBe` Right "asdf\nasdf"
      it "string with \\r" $
        decode parseString "\"asdf\\rasdf\"" `shouldBe` Right "asdf\rasdf"
      it "string with \\t" $
        decode parseString "\"asdf\\tasdf\"" `shouldBe` Right "asdf\tasdf"

  describe "array parsing" $ do
    describe "requiredElement" $ do
      let parser = parseArray $ requiredElement parseNull
      it "empty array" $
        decode parser "[]" `shouldSatisfy` isLeft
      it "single valuue array" $
        decode parser "[ null ]" `shouldBe` Right ()

    describe "optionalElement" $ do
      let parser = parseArray $ optionalElement Nothing $ fmap Just parseNull
      it "empty array" $
        decode parser "[]" `shouldBe` Right Nothing
      it "single valuue array" $
        decode parser "[ null ]" `shouldBe` Right (Just ())

    describe "arrayOf" $ do
      let nullArrayParser = parseArray $ arrayOf parseNull
      it "empty array" $
        decode nullArrayParser "[]" `shouldBe` Right []
      it "single value array" $
        decode nullArrayParser "[null]" `shouldBe` Right [()]
      it "longer array" $
        decode nullArrayParser "[null,null,null]" `shouldBe` Right [(),(),()]

      it "[false,true]" $ do
        let boolParser = (False <$ parseFalse) <> (True <$ parseTrue)
            boolArrayParser = parseArray $ arrayOf boolParser
        decode boolArrayParser "[false,true]" `shouldBe` Right [False,True]

  describe "object parsing" $ do
    describe "fail on unknown fields" $ do
      let parser = parseObject $ pure ()
      it "empty object" $
        decode parser "{}" `shouldBe` Right ()
      it "unexpected field" $
        decode parser "{\"name\":\"pesho\"}" `shouldSatisfy` isLeft

    describe "ignore unknown fields" $ do
      let parser = parseObject ignoreAnyField
      it "empty object" $
        decode parser "{}" `shouldBe` Right ()
      it "several fields" $
        decode parser "{\"name\":\"pesho\",\"age\":42}" `shouldBe` Right ()

    describe "capture unknown fields" $ do
      let parser = parseObject $ captureAllFields parseString
      it "empty object" $
        decode parser "{}" `shouldBe` Right []
      it "several fields" $
        decode parser "{\"name\":\"pesho\",\"label\":\"gosho\"}" `shouldBe` Right [("label","gosho"),("name","pesho")]

    describe "parsing required fields" $ do
      let parser = parseObject $ do
            x <- requiredField "name" parseString
            y <- requiredField "age"  parseNumber
            pure (x, y)
      it "fields present in order" $
        decode parser "{\"name\":\"pesho\",\"age\":42}" `shouldBe` Right ("pesho", 42)
      it "fields present out of order" $
        decode parser "{\"age\":42,\"name\":\"pesho\"}" `shouldBe` Right ("pesho", 42)
      it "missing field" $
        decode parser "{\"name\":\"pesho\"}" `shouldSatisfy` isLeft
      it "excess field" $
        decode parser "{\"age\":42,\"deleted\":false,\"name\":\"pesho\"}" `shouldSatisfy` isLeft

  it "spaces around tokens" $
    decode parseIgnore " [ false , true , null , 42 , { \"name\" : \"pesho\" }, [ ] ] " `shouldBe` Right ()
