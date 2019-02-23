{-# LANGUAGE TemplateHaskell #-}

module Faker.Pokemon where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Pokemon
import Faker.TH


$(generateFakeField "pokemon" "names")

$(generateFakeField "pokemon" "locations")

$(generateFakeField "pokemon" "moves")






