{-#LANGUAGE OverloadedStrings#-}

module Faker.Address where

import Data.Yaml
import Faker
import Config
import Data.Vector
import Control.Monad.Catch
import Data.Text
import System.Directory (doesFileExist)
import System.FilePath
import Control.Monad.IO.Class

parseCountry :: Value -> Parser (Vector Text)
parseCountry (Object obj) = do
  en <- obj .: "en"
  faker <- en .: "faker"
  address <- faker .: "address"
  country <- address .: "country"
  pure country
parseCountry val = fail $ "expected Object, but got " <> (show val)

addressFileEn :: FilePath
addressFileEn = localesEnDirectory </> "address.yml"

guessAddressFile :: Text -> FilePath
guessAddressFile sysloc = case sysloc of
                            "en" -> addressFileEn
                            oth  -> localesDirectory </> (unpack oth <> ".yml")

addressFile :: (MonadThrow m, MonadIO m) => FilePath -> m FilePath
addressFile fname = do
  exist <- liftIO $ doesFileExist fname
  if exist
  then pure fname
  else throwM $ InvalidLocale fname

countries :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
countries settings = do
  let fname = guessAddressFile (fakerLocale settings)
  afile <- addressFile fname
  yaml <- decodeFileThrow afile
  parseMonad parseCountry yaml

-- https://hackage.haskell.org/package/fake-0.1.1.1/docs/Fake.html
-- FGen type has to be modified to take a Config { cfLocale :: String, cfStdGen :: Gen}
