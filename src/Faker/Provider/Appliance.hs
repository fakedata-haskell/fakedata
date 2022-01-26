{-# LANGUAGE OverloadedStrings #-}

module Faker.Provider.Appliance where

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


parseAppliance :: FromJSON a => FakerSettings -> Value -> Parser a
parseAppliance settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  appliance <- faker .: "appliance"
  pure appliance
parseAppliance settings val = fail $ "expected Object, but got " <> (show val)

parseApplianceField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseApplianceField settings txt val = do
  appliance <- parseAppliance settings val
  field <- appliance .:? txt .!= mempty
  pure field

parseApplianceBrand ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseApplianceBrand settings = parseApplianceField settings "brand"

parseApplianceEquipment ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseApplianceEquipment settings = parseApplianceField settings "equipment"

applianceBrandProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
applianceBrandProvider settings =
  fetchData settings Appliance parseApplianceBrand

applianceEquipmentProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
applianceEquipmentProvider settings =
  fetchData settings Appliance parseApplianceEquipment
