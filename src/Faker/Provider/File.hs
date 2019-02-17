{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.File where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseFile :: FromJSON a => FakerSettings -> Value -> Parser a
parseFile settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  file <- faker .: "file"
  pure file
parseFile settings val = fail $ "expected Object, but got " <> (show val)

parseFileField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseFileField settings txt val = do
  file <- parseFile settings val
  field <- file .:? txt .!= mempty
  pure field

parseFileFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseFileFields settings txts val = do
  file <- parseFile settings val
  helper file txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "file" "extension")

$(genProvider "file" "extension")

$(genParser "file" "mime_type")

$(genProvider "file" "mime_type")
