{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Food where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Food
import Faker.TH

$(generateFakeField "food" "dish")

$(generateFakeField "food" "descriptions")

$(generateFakeField "food" "ingredients")

$(generateFakeField "food" "fruits")

$(generateFakeField "food" "vegetables")

$(generateFakeField "food" "spices")

$(generateFakeField "food" "measurements")

$(generateFakeField "food" "measurement_sizes")

$(generateFakeField "food" "metric_measurements")

$(generateFakeField "food" "sushi")
