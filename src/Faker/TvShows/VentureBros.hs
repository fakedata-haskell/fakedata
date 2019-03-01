{-# LANGUAGE TemplateHaskell #-}

module Faker.VentureBros where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.VentureBros
import Faker.TH


$(generateFakeField "ventureBros" "character")

$(generateFakeField "ventureBros" "organization")

$(generateFakeField "ventureBros" "vehicle")

$(generateFakeField "ventureBros" "quote")






