{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Food where

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

parseFood :: FromJSON a => FakerSettings -> Value -> Parser a
parseFood settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  food <- faker .: "food"
  pure food
parseFood settings val = fail $ "expected Object, but got " <> (show val)

parseFoodField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseFoodField settings txt val = do
  food <- parseFood settings val
  field <- food .:? txt .!= mempty
  pure field

parseFoodFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseFoodFields settings txts val = do
  food <- parseFood settings val
  helper food txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "food" "dish")

$(genProvider "food" "dish")

$(genParser "food" "descriptions")

$(genProvider "food" "descriptions")

$(genParser "food" "ingredients")

$(genProvider "food" "ingredients")

$(genParser "food" "fruits")

$(genProvider "food" "fruits")

$(genParser "food" "vegetables")

$(genProvider "food" "vegetables")

$(genParser "food" "spices")

$(genProvider "food" "spices")

$(genParser "food" "measurements")

$(genProvider "food" "measurements")

$(genParser "food" "measurement_sizes")

$(genProvider "food" "measurement_sizes")

$(genParser "food" "metric_measurements")

$(genProvider "food" "metric_measurements")

$(genParser "food" "sushi")

$(genProvider "food" "sushi")
