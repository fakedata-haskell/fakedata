{-# LANGUAGE TemplateHaskell #-}

module Faker.FamilyGuy where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.FamilyGuy
import Faker.TH

$(generateFakeField "familyGuy" "character")

$(generateFakeField "familyGuy" "location")

$(generateFakeField "familyGuy" "quote")
