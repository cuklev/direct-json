{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -Wno-error=orphans #-}
import System.Environment (getArgs, getExecutablePath)
import System.Process.Typed
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Monad (unless)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import Data.Aeson (eitherDecode, FromJSON (..), withObject, (.:))
import Text.Json.Decode (decode, parseIgnore, parseFalse, parseTrue, parseString, parseObject, requiredField, parseArray, arrayOf, parseNumber)

main :: IO ()
main = getArgs >>= \case
  ["run", "direct-ignore", n] -> runDirectIgnore $ testData $ read n
  ["run", "direct", n]        -> runDirect       $ testData $ read n
  ["run", "aeson",  n]        -> runAeson        $ testData $ read n
  [] -> runAll
  _  -> fail "Invalid arguments"

runAll :: IO ()
runAll = do
  exe <- getExecutablePath

  let time :: String -> Int -> IO T.Text
      time t n = do
        begin <- getCurrentTime
        runProcess_ $ proc exe ["run", t, show n]
        end   <- getCurrentTime
        pure $ T.pack $ show $ end `diffUTCTime` begin

      mem :: String -> Int -> IO T.Text
      mem t n = do
        output <- readProcessStderr_ $ proc exe ["run", t, show n, "+RTS", "-s"]
        pure $ fromMaybe "Can not parse memory usage" $ do
          [p1, _] <- Just $ T.splitOn "total memory in use" $ T.decodeUtf8 $ BSL.toStrict output
          [w2, w1] <- Just $ take 2 $ reverse $ T.words p1
          Just $ w1 <> w2

  let libs = ["direct-ignore", "direct", "aeson"]
  putStrLn $ "Size"
          ++ concatMap (\lib -> ',' : lib ++ " (time)") libs
          ++ concatMap (\lib -> ',' : lib ++ " (mem)") libs

  for_ [0, 1, 5, 10, 20, 40, 60, 80, 100] $ \size -> do
    putStr $ show size
    for_ [time, mem] $ \bench ->
      for_ libs $ \lib -> do
        result <- bench lib size
        T.putStr $ "," <> result
    T.putStrLn ""

runDirectIgnore :: (BSL.ByteString, [[BigObject]]) -> IO ()
runDirectIgnore (inputStr, _) = either fail pure $ decode parseIgnore inputStr

runDirect :: (BSL.ByteString, [[BigObject]]) -> IO ()
runDirect (inputStr, inputObj) = do
  let boolParser = (False <$ parseFalse) <> (True <$ parseTrue)
      textParser = fmap BSL.toStrict parseString
      smallObject = parseObject $ do
        smallName <- requiredField "name" textParser
        smallAge  <- floor <$> requiredField "age" parseNumber
        smallDeleted <- requiredField "deleted" boolParser
        pure SmallObject{..}
      bigObject = parseObject $ do
        bigLabel <- requiredField "label" textParser
        bigData  <- requiredField "data" $ V.fromList <$> parseArray (arrayOf smallObject)
        pure BigObject{..}
      bigs = parseArray $ arrayOf bigObject
      parser = parseArray $ arrayOf bigs
  result <- either fail pure $ decode parser inputStr
  unless (result == inputObj) $ fail "Decoding error"

runAeson :: (BSL.ByteString, [[BigObject]]) -> IO ()
runAeson (inputStr, inputObj) = do
  result <- either fail pure $ eitherDecode inputStr
  unless (result == inputObj) $ fail "Decoding error"

testData :: Int -> (BSL.ByteString, [[BigObject]])
testData size = (str, obj)
  where
    str = array size $ array size bigObject
      where
        bigObject = "{\"data\":" <> array size smallObject <> ",\"label\":\"ivan\"}"
        smallObject = "{\"name\":\"pesho\",\"age\":42,\"deleted\":true}"
        array n inner = "[" <> BSL.intercalate "," (replicate n inner) <> "]"

    obj = replicate size $ replicate size bigObject
      where
        bigObject = BigObject "ivan" $ V.replicate size smallObject
        smallObject = SmallObject "pesho" 42 True

data SmallObject = SmallObject
  { smallName    :: {-# UNPACK #-} !BS.ByteString
  , smallAge     :: {-# UNPACK #-} !Int
  , smallDeleted ::                !Bool
  }
  deriving Eq

instance FromJSON SmallObject where
  parseJSON = withObject "SmallObject" $ \o ->
    SmallObject
      <$> o .: "name"
      <*> o .: "age"
      <*> o .: "deleted"

data BigObject = BigObject
  { bigLabel :: {-# UNPACK #-} !BS.ByteString
  , bigData  :: {-# UNPACK #-} !(V.Vector SmallObject)
  }
  deriving Eq

instance FromJSON BigObject where
  parseJSON = withObject "BigObject" $ \o ->
    BigObject
      <$> o .: "label"
      <*> o .: "data"

instance FromJSON BS.ByteString where
  parseJSON = fmap T.encodeUtf8 . parseJSON
