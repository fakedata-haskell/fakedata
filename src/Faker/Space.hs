{-# LANGUAGE TemplateHaskell #-}

module Faker.Space where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Space
import Faker.TH


$(generateFakeField "space" "planet")

$(generateFakeField "space" "moon")

$(generateFakeField "space" "galaxy")

$(generateFakeField "space" "nebula")

$(generateFakeField "space" "star_cluster")

$(generateFakeField "space" "constellation")

$(generateFakeField "space" "star")

$(generateFakeField "space" "agency")

$(generateFakeField "space" "agency_abv")

$(generateFakeField "space" "nasa_space_craft")

$(generateFakeField "space" "company")

$(generateFakeField "space" "distance_measurement")

$(generateFakeField "space" "meteorite")

$(generateFakeField "space" "launch_vehicule")






