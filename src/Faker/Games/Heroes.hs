{-# LANGUAGE TemplateHaskell #-}

module Faker.Games.Heroes where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Heroes
import Faker.TH

$(generateFakeField "heroes" "names")

$(generateFakeField "heroes" "specialties")

$(generateFakeField "heroes" "klasses")
