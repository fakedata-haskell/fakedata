{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Drone where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Monoid ((<>))
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH
import qualified Data.Aeson.Key as K

parseDrone :: FromJSON a => FakerSettings -> Value -> Parser a
parseDrone settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  drone <- faker .: "drone"
  pure drone
parseDrone settings val = fail $ "expected Object, but got " <> (show val)

parseDroneField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseDroneField settings txt val = do
  drone <- parseDrone settings val
  field <- drone .:? txt .!= mempty
  pure field

parseDroneFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseDroneFields settings txts val = do
  drone <- parseDrone settings val
  helper drone txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)


parseUnresolvedDroneField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> K.Key
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedDroneField settings txt val = do
  drone <- parseDrone settings val
  field <- drone .:? txt .!= mempty
  pure $ pure field



parseUnresolvedDroneFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [K.Key]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedDroneFields settings txts val = do
  drone <- parseDrone settings val
  helper drone txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)





$(genParser "drone" "name")
$(genProvider "drone" "name")


$(genParser "drone" "battery_type")
$(genProvider "drone" "battery_type")

$(genParser "drone" "iso")
$(genProvider "drone" "iso")

$(genParser "drone" "photo_format")
$(genProvider "drone" "photo_format")

$(genParser "drone" "video_format")
$(genProvider "drone" "video_format")

$(genParser "drone" "max_shutter_speed")
$(genProvider "drone" "max_shutter_speed")

$(genParser "drone" "min_shutter_speed")
$(genProvider "drone" "min_shutter_speed")

$(genParser "drone" "shutter_speed_units")
$(genProvider "drone" "shutter_speed_units")

$(genParserSingleUnresolved "drone" "weight")
$(genProvidersSingleUnresolved "drone" ["weight"])

$(genParserSingleUnresolved "drone" "max_ascent_speed")
$(genProvidersSingleUnresolved "drone" ["max_ascent_speed"])

$(genParserSingleUnresolved "drone" "max_descent_speed")
$(genProvidersSingleUnresolved "drone" ["max_descent_speed"])

$(genParserSingleUnresolved "drone" "flight_time")
$(genProvidersSingleUnresolved "drone" ["flight_time"])

$(genParserSingleUnresolved "drone" "max_altitude")
$(genProvidersSingleUnresolved "drone" ["max_altitude"])

$(genParserSingleUnresolved "drone" "max_flight_distance")
$(genProvidersSingleUnresolved "drone" ["max_flight_distance"])

$(genParserSingleUnresolved "drone" "max_speed")
$(genProvidersSingleUnresolved "drone" ["max_speed"])

$(genParserSingleUnresolved "drone" "max_wind_resistance")
$(genProvidersSingleUnresolved "drone" ["max_wind_resistance"])

$(genParserSingleUnresolved "drone" "max_angular_velocity")
$(genProvidersSingleUnresolved "drone" ["max_angular_velocity"])

$(genParserSingleUnresolved "drone" "max_tilt_angle")
$(genProvidersSingleUnresolved "drone" ["max_tilt_angle"])

$(genParserSingleUnresolved "drone" "operating_temperature")
$(genProvidersSingleUnresolved "drone" ["operating_temperature"])

$(genParserUnresolved "drone" "battery_capacity")
$(genProviderUnresolveds "drone" ["battery_capacity"])

$(genParserSingleUnresolved "drone" "battery_voltage")
$(genProvidersSingleUnresolved "drone" ["battery_voltage"])

$(genParserSingleUnresolved "drone" "battery_weight")
$(genProvidersSingleUnresolved "drone" ["battery_weight"])

$(genParserSingleUnresolved "drone" "charging_temperature")
$(genProvidersSingleUnresolved "drone" ["charging_temperature"])

$(genParserSingleUnresolved "drone" "max_charging_power")
$(genProvidersSingleUnresolved "drone" ["max_charging_power"])

$(genParserSingleUnresolved "drone" "max_resolution")
$(genProvidersSingleUnresolved "drone" ["max_resolution"])



resolveDroneText :: (MonadIO m, MonadThrow m) => FakerSettings -> K.Key -> m Text
resolveDroneText = genericResolver' resolveDroneField

resolveDroneField :: (MonadThrow m, MonadIO m) => FakerSettings -> K.Key -> m Text
resolveDroneField settings str = throwM $ InvalidField "drone" str
