{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Cannabis where

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

parseCannabis :: FromJSON a => FakerSettings -> Value -> Parser a
parseCannabis settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  cannabis <- faker .: "cannabis"
  pure cannabis
parseCannabis settings val = fail $ "expected Object, but got " <> (show val)

parseCannabisField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseCannabisField settings txt val = do
  cannabis <- parseCannabis settings val
  field <- cannabis .:? txt .!= mempty
  pure field

$(genParser "cannabis" "strains")

$(genProvider "cannabis" "strains")

$(genParser "cannabis" "cannabinoid_abbreviations")

$(genProvider "cannabis" "cannabinoid_abbreviations")

$(genParser "cannabis" "cannabinoids")

$(genProvider "cannabis" "cannabinoids")

$(genParser "cannabis" "terpenes")

$(genProvider "cannabis" "terpenes")

$(genParser "cannabis" "medical_uses")

$(genProvider "cannabis" "medical_uses")

$(genParser "cannabis" "health_benefits")

$(genProvider "cannabis" "health_benefits")

$(genParser "cannabis" "categories")

$(genProvider "cannabis" "categories")

$(genParser "cannabis" "types")

$(genProvider "cannabis" "types")

$(genParser "cannabis" "buzzwords")

$(genProvider "cannabis" "buzzwords")
