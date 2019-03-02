{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.HarryPotter where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseHarryPotter :: FromJSON a => FakerSettings -> Value -> Parser a
parseHarryPotter settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  harryPotter <- faker .: "harry_potter"
  pure harryPotter
parseHarryPotter settings val = fail $ "expected Object, but got " <> (show val)

parseHarryPotterField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseHarryPotterField settings txt val = do
  harryPotter <- parseHarryPotter settings val
  field <- harryPotter .:? txt .!= mempty
  pure field

parseHarryPotterFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseHarryPotterFields settings txts val = do
  harryPotter <- parseHarryPotter settings val
  helper harryPotter txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "harryPotter" "characters")

$(genProvider "harryPotter" "characters")

$(genParser "harryPotter" "locations")

$(genProvider "harryPotter" "locations")

$(genParser "harryPotter" "quotes")

$(genProvider "harryPotter" "quotes")

$(genParser "harryPotter" "books")

$(genProvider "harryPotter" "books")

$(genParser "harryPotter" "houses")

$(genProvider "harryPotter" "houses")

$(genParser "harryPotter" "spells")

$(genProvider "harryPotter" "spells")
