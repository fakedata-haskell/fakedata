{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Game.HeroesOfTheStorm where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.HeroesOfTheStorm
import Faker.TH

$(generateFakeField "heroesOfTheStorm" "battlegrounds")

-- @since 0.8.0
--
$(generateFakeField "heroesOfTheStorm" "class_names")

$(generateFakeField "heroesOfTheStorm" "heroes")

$(generateFakeField "heroesOfTheStorm" "quotes")
