{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Construction where

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
import qualified Data.Aeson.Key as K

parseConstruction :: FromJSON a => FakerSettings -> Value -> Parser a
parseConstruction settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  construction <- faker .: "construction"
  pure construction
parseConstruction settings val =
  fail $ "expected Object, but got " <> (show val)

parseConstructionField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseConstructionField settings txt val = do
  construction <- parseConstruction settings val
  field <- construction .:? txt .!= mempty
  pure field

$(genParser "construction" "materials")

$(genProvider "construction" "materials")

$(genParser "construction" "subcontract_categories")

$(genProvider "construction" "subcontract_categories")

$(genParser "construction" "heavy_equipment")

$(genProvider "construction" "heavy_equipment")

$(genParser "construction" "roles")

$(genProvider "construction" "roles")

$(genParser "construction" "trades")

$(genProvider "construction" "trades")

$(genParser "construction" "standard_cost_codes")

$(genProvider "construction" "standard_cost_codes")
