{-# LANGUAGE TemplateHaskell #-}

module Faker.Games.HalfLife where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.HalfLife
import Faker.TH

$(generateFakeField "halfLife" "character")

$(generateFakeField "halfLife" "enemy")

$(generateFakeField "halfLife" "location")
