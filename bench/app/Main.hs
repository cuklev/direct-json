{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
import System.Environment (getArgs, getExecutablePath)
import System.Process.Typed
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Monad (unless)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V
-- import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Aeson (eitherDecode, FromJSON (..), withObject, (.:))
import Text.Json.Decode (decode, parseIgnore, parseFalse, parseTrue, parseString, parseObject, requiredField, parseNumber, parseArray, arrayOf)

main :: IO ()
main = getArgs >>= \case
  ["run", "direct"] -> runDirect
  ["run", "aeson"]  -> runAeson
  []                -> runAll
  _                 -> fail "Invalid arguments"

runAll :: IO ()
runAll = do
  exe <- getExecutablePath

  let time :: String -> IO ()
      time t = do
        putStr $ "Testing " ++ t ++ ": "
        begin <- getCurrentTime
        runProcess_ $ proc exe ["run", t]
        end   <- getCurrentTime
        print $ end `diffUTCTime` begin

  time "direct"
  time "aeson"

runDirect :: IO ()
runDirect = do
  either fail pure $ decode parseIgnore inputStr

runAeson :: IO ()
runAeson = do
  result <- either fail pure $ eitherDecode inputStr
  unless (result == inputObj) $ fail "Decoding error"

inputStr :: BSL.ByteString
inputStr = array 100 $ array 100 bigObject
  where
    bigObject = "{\"data\":" <> array 100 smallObject <> ",\"label\":\"ivan\"}"
    smallObject = "{\"name\":\"pesho\",\"age\":42,\"deleted\":true}"
    array n inner = "[" <> BSL.intercalate "," (replicate n inner) <> "]"

inputObj :: [[BigObject]]
inputObj = replicate 100 $ replicate 100 bigObject
  where
    bigObject = BigObject "ivan" $ V.replicate 100 smallObject
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

type Input = V.Vector (V.Vector BigObject)

instance FromJSON BS.ByteString where
  parseJSON = fmap T.encodeUtf8 . parseJSON


directParser = parser
  where
    boolParser = (False <$ parseFalse) <> (True <$ parseTrue)
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
    bigs = V.fromList <$> parseArray (arrayOf bigObject)
    parser = V.fromList <$> parseArray (arrayOf bigs)
