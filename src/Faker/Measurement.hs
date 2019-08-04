{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Measurement where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Measurement
import Faker.TH

$(generateFakeField "measurement" "height")

$(generateFakeField "measurement" "length")

$(generateFakeField "measurement" "volume")

$(generateFakeField "measurement" "weight")

$(generateFakeField "measurement" "metric_height")

$(generateFakeField "measurement" "metric_length")

$(generateFakeField "measurement" "metric_volume")

$(generateFakeField "measurement" "metric_weight")
