{-# LANGUAGE TemplateHaskell #-}

module Faker.Game.HeroesOfTheStorm where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.HeroesOfTheStorm
import Faker.TH

$(generateFakeField "heroesOfTheStorm" "battlegrounds")

$(generateFakeField "heroesOfTheStorm" "classes")

$(generateFakeField "heroesOfTheStorm" "heroes")

$(generateFakeField "heroesOfTheStorm" "quotes")
