{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -Wno-error=unused-top-binds #-}
{-# OPTIONS -Wno-error=orphans #-}
import System.Environment (getArgs, getExecutablePath)
import System.Process.Typed
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
        begin <- getCurrentTime
        runProcess_ $ proc exe ["run", t]
        end   <- getCurrentTime
        putStrLn $ "Time: " ++ show (end `diffUTCTime` begin)

      mem :: String -> IO ()
      mem t = do
        output <- readProcessStderr_ $ proc exe ["run", t, "+RTS", "-s"]
        T.putStrLn $ fromMaybe "Can not parse memory usage" $ do
          [p1, _] <- Just $ T.splitOn "total memory in use" $ T.decodeUtf8 $ BSL.toStrict output
          [w2, w1] <- Just $ take 2 $ reverse $ T.words p1
          Just $ "Memory: " <> w1 <> " " <> w2

  putStrLn "Testing direct-json:"
  time "direct"
  mem "direct"

  putStrLn "Testing aeson:"
  time "aeson"
  mem "aeson"

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

instance FromJSON BS.ByteString where
  parseJSON = fmap T.encodeUtf8 . parseJSON
