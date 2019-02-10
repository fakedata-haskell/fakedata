{-# LANGUAGE TemplateHaskell #-}

module Faker.CultureSeries where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.CultureSeries
import Faker.TH

$(generateFakeField "cultureSeries" "books")

$(generateFakeField "cultureSeries" "culture_ships")

$(generateFakeField "cultureSeries" "culture_ship_classes")

$(generateFakeField "cultureSeries" "culture_ship_class_abvs")

$(generateFakeField "cultureSeries" "civs")

$(generateFakeField "cultureSeries" "planets")
