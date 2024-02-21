{-# LANGUAGE OverloadedStrings #-}
module Text.Json.Encode
  ( encodeNull
  , encodeFalse
  , encodeTrue
  , encodeNumber
  , encodeString
  , encodeArray
  , encodeObject
  , encode
  , JsonEncode (..)
  , encodeC
  ) where

import Data.Binary.Builder (toLazyByteString)
import Data.ByteString.Builder.Scientific (scientificBuilder)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (chr, ord)
import Data.Scientific (Scientific, coefficient, base10Exponent)

newtype ValueEncoder = ValueEncoder { encoderChunks :: [BS.ByteString] }

encodeNull :: ValueEncoder
encodeNull = ValueEncoder ["null"]

encodeFalse :: ValueEncoder
encodeFalse = ValueEncoder ["false"]

encodeTrue :: ValueEncoder
encodeTrue = ValueEncoder ["true"]

encodeNumber :: Scientific -> ValueEncoder
encodeNumber s
  | e < 0 || e > 6 = ValueEncoder [BSL.toStrict $ toLazyByteString $ scientificBuilder s]
  | otherwise      = ValueEncoder [BS.pack $ show $ coefficient s * 10 ^ e]
  where e = base10Exponent s

encodeString :: BSL.ByteString -> ValueEncoder
encodeString str = ValueEncoder $ "\"" : strEncode str ++ ["\""]
  where
    strEncode :: BSL.ByteString -> [BS.ByteString]
    strEncode str1 =
      let (unescaped, rest) = BSL.span (not . mustEscape) str1
      in BSL.toChunks unescaped ++ case BSL.uncons rest of
            Nothing      -> []
            Just (x, xs) -> escape x : strEncode xs

    mustEscape c = c == '"' || c == '\\' || ord c < 32

    escape '"'  = "\\\""
    escape '\\' = "\\\\"
    escape '\b' = "\\b"
    escape '\f' = "\\f"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c    = "\\u00" <> BS.pack [chr c1, chr c2]
      where d = ord c
            (d1, d2) = d `divMod` 16
            c1 = ord '0' + d1
            c2 | d2 < 10 = ord '0' + d2
               | otherwise = ord 'A' + d2 - 10

encodeArray :: [ValueEncoder] -> ValueEncoder
encodeArray [] = ValueEncoder ["[]"]
encodeArray (x:xs) = ValueEncoder
  $ "[" : encoderChunks x
 ++ concatMap ((",":) . encoderChunks) xs
 ++ ["]"]

encodeObject :: [(BSL.ByteString, ValueEncoder)] -> ValueEncoder
encodeObject [] = ValueEncoder ["{}"]
encodeObject (x:xs) = ValueEncoder
  $ "{" : encodePair x
 ++ concatMap ((",":) . encodePair) xs
 ++ ["}"]
  where encodePair (key, value) = encoderChunks (encodeString key) ++ ":" : encoderChunks value

encode :: ValueEncoder -> BSL.ByteString
encode = BSL.fromChunks . encoderChunks

class JsonEncode a where
  jsonEncode :: a -> ValueEncoder

encodeC :: JsonEncode a => a -> BSL.ByteString
encodeC = encode . jsonEncode
