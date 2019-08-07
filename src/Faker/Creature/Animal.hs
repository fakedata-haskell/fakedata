{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Creature.Animal where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Animal
import Faker.TH

$(generateFakeField "animal" "name")
