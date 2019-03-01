{-# LANGUAGE TemplateHaskell #-}

module Faker.Creature.Cat where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Cat
import Faker.TH

$(generateFakeField "cat" "name")

$(generateFakeField "cat" "breed")

$(generateFakeField "cat" "registry")
