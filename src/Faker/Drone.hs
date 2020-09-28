{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Drone where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Drone
import Faker.TH

$(generateFakeField "drone" "name")

$(generateFakeField "drone" "battery_type")

$(generateFakeField "drone" "iso")

$(generateFakeField "drone" "photo_format")

$(generateFakeField "drone" "video_format")

$(generateFakeField "drone" "max_shutter_speed")

$(generateFakeField "drone" "min_shutter_speed")

$(generateFakeField "drone" "shutter_speed_units")

$(generateFakeFieldSingleUnresolved "drone" "weight")

$(generateFakeFieldSingleUnresolved "drone" "max_ascent_speed")

$(generateFakeFieldSingleUnresolved "drone" "max_descent_speed")

$(generateFakeFieldSingleUnresolved "drone" "flight_time")

$(generateFakeFieldSingleUnresolved "drone" "max_altitude")

$(generateFakeFieldSingleUnresolved "drone" "max_flight_distance")

$(generateFakeFieldSingleUnresolved "drone" "max_speed")

$(generateFakeFieldSingleUnresolved "drone" "max_wind_resistance")

$(generateFakeFieldSingleUnresolved "drone" "max_angular_velocity")

$(generateFakeFieldSingleUnresolved "drone" "max_tilt_angle")

$(generateFakeFieldSingleUnresolved "drone" "operating_temperature")

$(generateFakeFieldUnresolved "drone" "battery_capacity")

$(generateFakeFieldSingleUnresolved "drone" "battery_voltage")

$(generateFakeFieldSingleUnresolved "drone" "battery_weight")

$(generateFakeFieldSingleUnresolved "drone" "charging_temperature")

$(generateFakeFieldSingleUnresolved "drone" "max_charging_power")

$(generateFakeFieldSingleUnresolved "drone" "max_resolution")



