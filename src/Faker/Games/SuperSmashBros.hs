{-# LANGUAGE TemplateHaskell #-}

module Faker.Games.SuperSmashBros where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.SuperSmashBros
import Faker.TH

$(generateFakeField "superSmashBros" "fighter")

$(generateFakeField "superSmashBros" "stage")
