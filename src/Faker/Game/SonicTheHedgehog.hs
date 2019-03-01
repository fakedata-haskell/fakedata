{-# LANGUAGE TemplateHaskell #-}

module Faker.Game.SonicTheHedgehog where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.SonicTheHedgehog
import Faker.TH

$(generateFakeField "sonicTheHedgehog" "zone")

$(generateFakeField "sonicTheHedgehog" "character")

$(generateFakeField "sonicTheHedgehog" "game")
