{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Vehicle where

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
import qualified Data.Aeson.Key as K

parseVehicle :: FromJSON a => FakerSettings -> Value -> Parser a
parseVehicle settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  vehicle <- faker .: "vehicle"
  pure vehicle
parseVehicle settings val = fail $ "expected Object, but got " <> (show val)

parseVehicleField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseVehicleField settings txt val = do
  vehicle <- parseVehicle settings val
  field <- vehicle .:? txt .!= mempty
  pure field

parseVehicleFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseVehicleFields settings txts val = do
  vehicle <- parseVehicle settings val
  helper vehicle txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

parseUnresolvedVehicleField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> K.Key
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedVehicleField settings txt val = do
  vehicle <- parseVehicle settings val
  field <- vehicle .:? txt .!= mempty
  pure $ pure field

$(genParser "vehicle" "manufacture")

$(genProvider "vehicle" "manufacture")

$(genParser "vehicle" "makes")

$(genProvider "vehicle" "makes")

$(genParser "vehicle" "colors")

$(genProvider "vehicle" "colors")

$(genParser "vehicle" "transmissions")

$(genProvider "vehicle" "transmissions")

$(genParser "vehicle" "drive_types")

$(genProvider "vehicle" "drive_types")

$(genParser "vehicle" "fuel_types")

$(genProvider "vehicle" "fuel_types")

$(genParser "vehicle" "styles")

$(genProvider "vehicle" "styles")

$(genParser "vehicle" "car_types")

$(genProvider "vehicle" "car_types")

$(genParser "vehicle" "car_options")

$(genProvider "vehicle" "car_options")

$(genParser "vehicle" "standard_specs")

$(genProvider "vehicle" "standard_specs")

$(genParser "vehicle" "doors")

vehicleDoorsProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Int)
vehicleDoorsProvider settings = fetchData settings Vehicle parseVehicleDoors

$(genParser "vehicle" "engine_sizes")

vehicleEngineSizesProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Int)
vehicleEngineSizesProvider settings =
  fetchData settings Vehicle parseVehicleEngineSizes

$(genParserUnresolved "vehicle" "license_plate")

-- $(genProviderUnresolved "vehicle" "license_plate")
vehicleLicensePlateProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
vehicleLicensePlateProvider _ = pure $ pure $ pure "???-###"

resolveVehicleText ::
     (MonadIO m, MonadThrow m) => FakerSettings -> K.Key -> m Text
resolveVehicleText settings txt = genericResolver settings txt resolveVehicleField

resolveVehicleField ::
     (MonadThrow m, MonadIO m) => FakerSettings -> K.Key -> m Text
resolveVehicleField settings str = throwM $ InvalidField "vehicle" str

$(genParsers "vehicle" ["models_by_make", "BMW"])

$(genProviders "vehicle" ["models_by_make", "BMW"])

$(genParsers "vehicle" ["models_by_make", "Audi"])

$(genProviders "vehicle" ["models_by_make", "Audi"])

$(genParsers "vehicle" ["models_by_make", "Toyota"])

$(genProviders "vehicle" ["models_by_make", "Toyota"])

$(genParsers "vehicle" ["models_by_make", "Chevy"])

$(genProviders "vehicle" ["models_by_make", "Chevy"])

$(genParsers "vehicle" ["models_by_make", "Ford"])

$(genProviders "vehicle" ["models_by_make", "Ford"])

$(genParsers "vehicle" ["models_by_make", "Dodge"])

$(genProviders "vehicle" ["models_by_make", "Dodge"])

$(genParsers "vehicle" ["models_by_make", "Lincoln"])

$(genProviders "vehicle" ["models_by_make", "Lincoln"])

$(genParsers "vehicle" ["models_by_make", "Buick"])

$(genProviders "vehicle" ["models_by_make", "Buick"])

$(genParsers "vehicle" ["models_by_make", "Honda"])

$(genProviders "vehicle" ["models_by_make", "Honda"])

$(genParsers "vehicle" ["models_by_make", "Nissan"])

$(genProviders "vehicle" ["models_by_make", "Nissan"])
