{-# LANGUAGE TemplateHaskell #-}

-- | @since 0.2.0
module Faker.Creature.Horse where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Horse
import Faker.TH

$(generateFakeField "horse" "name")

$(generateFakeField "horse" "breed")
