{-# LANGUAGE TemplateHaskell #-}

module Faker.Vehicle where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Vehicle
import Faker.TH

$(generateFakeField "vehicle" "manufacture")

$(generateFakeField "vehicle" "makes")

$(generateFakeField "vehicle" "colors")

$(generateFakeField "vehicle" "transmissions")

$(generateFakeField "vehicle" "drive_types")

$(generateFakeField "vehicle" "fuel_types")

$(generateFakeField "vehicle" "styles")

$(generateFakeField "vehicle" "car_types")

$(generateFakeField "vehicle" "car_options")

$(generateFakeField "vehicle" "standard_specs")

$(generateFakeField "vehicle" "doors")

$(generateFakeField "vehicle" "engine_sizes")

$(generateFakeFieldUnresolved "vehicle" "license_plate")

$(generateFakeFields "vehicle" ["models_by_make", "BMW"])

$(generateFakeFields "vehicle" ["models_by_make", "Audi"])

$(generateFakeFields "vehicle" ["models_by_make", "Toyota"])

$(generateFakeFields "vehicle" ["models_by_make", "Chevy"])

$(generateFakeFields "vehicle" ["models_by_make", "Ford"])

$(generateFakeFields "vehicle" ["models_by_make", "Dodge"])

$(generateFakeFields "vehicle" ["models_by_make", "Lincoln"])

$(generateFakeFields "vehicle" ["models_by_make", "Buick"])

$(generateFakeFields "vehicle" ["models_by_make", "Honda"])

$(generateFakeFields "vehicle" ["models_by_make", "Nissan"])
