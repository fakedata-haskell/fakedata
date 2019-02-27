{-# LANGUAGE TemplateHaskell #-}

module Faker.Restaurant where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Restaurant
import Faker.TH

$(generateFakeField "restaurant" "name_suffix")

$(generateFakeField "restaurant" "type")

$(generateFakeField "restaurant" "description")

$(generateFakeField "restaurant" "review")

$(generateFakeFieldUnresolved "restaurant" "name_prefix")

$(generateFakeFieldUnresolved "restaurant" "name")
