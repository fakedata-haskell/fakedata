{-# LANGUAGE TemplateHaskell #-}

module Faker.Movie.LordOfTheRings where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.LordOfTheRings
import Faker.TH

$(generateFakeField "lordOfTheRings" "characters")

$(generateFakeField "lordOfTheRings" "locations")

$(generateFakeField "lordOfTheRings" "quotes")
