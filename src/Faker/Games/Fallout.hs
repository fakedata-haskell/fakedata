{-# LANGUAGE TemplateHaskell #-}

module Faker.Fallout where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Fallout
import Faker.TH

$(generateFakeField "fallout" "characters")

$(generateFakeField "fallout" "factions")

$(generateFakeField "fallout" "locations")

$(generateFakeField "fallout" "quotes")
