{-# LANGUAGE TemplateHaskell #-}

module Faker.StarTrek where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.StarTrek
import Faker.TH


$(generateFakeField "starTrek" "character")

$(generateFakeField "starTrek" "location")

$(generateFakeField "starTrek" "specie")

$(generateFakeField "starTrek" "villain")






