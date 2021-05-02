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
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL

newtype ValueEncoder = ValueEncoder { encoderChunks :: [BS.ByteString] }

encodeNull :: ValueEncoder
encodeNull = ValueEncoder ["null"]

encodeFalse :: ValueEncoder
encodeFalse = ValueEncoder ["false"]

encodeTrue :: ValueEncoder
encodeTrue = ValueEncoder ["true"]

encodeNumber :: Int -> ValueEncoder
encodeNumber = ValueEncoder . pure . BS.pack . show

encodeString :: BSL.ByteString -> ValueEncoder
encodeString str = ValueEncoder $ "\"" : BSL.toChunks str ++ ["\""]

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
