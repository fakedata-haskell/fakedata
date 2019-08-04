{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.CultureSeries where

import Config
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

parseCultureSeries :: FromJSON a => FakerSettings -> Value -> Parser a
parseCultureSeries settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  cultureSeries <- faker .: "culture_series"
  pure cultureSeries
parseCultureSeries settings val =
  fail $ "expected Object, but got " <> (show val)

parseCultureSeriesField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseCultureSeriesField settings txt val = do
  cultureSeries <- parseCultureSeries settings val
  field <- cultureSeries .:? txt .!= mempty
  pure field

$(genParser "cultureSeries" "books")

$(genProvider "cultureSeries" "books")

$(genParser "cultureSeries" "culture_ships")

$(genProvider "cultureSeries" "culture_ships")

$(genParser "cultureSeries" "culture_ship_classes")

$(genProvider "cultureSeries" "culture_ship_classes")

$(genParser "cultureSeries" "culture_ship_class_abvs")

$(genProvider "cultureSeries" "culture_ship_class_abvs")

$(genParser "cultureSeries" "civs")

$(genProvider "cultureSeries" "civs")

$(genParser "cultureSeries" "planets")

$(genProvider "cultureSeries" "planets")
