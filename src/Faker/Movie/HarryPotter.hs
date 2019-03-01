{-# LANGUAGE TemplateHaskell #-}

module Faker.HarryPotter where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.HarryPotter
import Faker.TH


$(generateFakeField "harryPotter" "characters")

$(generateFakeField "harryPotter" "locations")

$(generateFakeField "harryPotter" "quotes")

$(generateFakeField "harryPotter" "books")

$(generateFakeField "harryPotter" "houses")

$(generateFakeField "harryPotter" "spells")






