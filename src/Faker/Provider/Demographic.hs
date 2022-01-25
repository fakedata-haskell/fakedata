{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Demographic where

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

parseDemographic :: FromJSON a => FakerSettings -> Value -> Parser a
parseDemographic settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  demographic <- faker .: "demographic"
  pure demographic
parseDemographic settings val = fail $ "expected Object, but got " <> (show val)

parseDemographicField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseDemographicField settings txt val = do
  demographic <- parseDemographic settings val
  field <- demographic .:? txt .!= mempty
  pure field

$(genParser "demographic" "race")

$(genProvider "demographic" "race")

$(genParser "demographic" "sex")

$(genProvider "demographic" "sex")

$(genParser "demographic" "demonym")

$(genProvider "demographic" "demonym")

$(genParser "demographic" "educational_attainment")

$(genProvider "demographic" "educational_attainment")

$(genParser "demographic" "marital_status")

$(genProvider "demographic" "marital_status")
