{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Measurement where

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


parseMeasurement :: FromJSON a => FakerSettings -> Value -> Parser a
parseMeasurement settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  measurement <- faker .: "measurement"
  pure measurement
parseMeasurement settings val = fail $ "expected Object, but got " <> (show val)

parseMeasurementField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseMeasurementField settings txt val = do
  measurement <- parseMeasurement settings val
  field <- measurement .:? txt .!= mempty
  pure field

parseMeasurementFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseMeasurementFields settings txts val = do
  measurement <- parseMeasurement settings val
  helper measurement txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "measurement" "height")

$(genProvider "measurement" "height")

$(genParser "measurement" "length")

$(genProvider "measurement" "length")

$(genParser "measurement" "volume")

$(genProvider "measurement" "volume")

$(genParser "measurement" "weight")

$(genProvider "measurement" "weight")

$(genParser "measurement" "metric_height")

$(genProvider "measurement" "metric_height")

$(genParser "measurement" "metric_length")

$(genProvider "measurement" "metric_length")

$(genParser "measurement" "metric_volume")

$(genProvider "measurement" "metric_volume")

$(genParser "measurement" "metric_weight")

$(genProvider "measurement" "metric_weight")
