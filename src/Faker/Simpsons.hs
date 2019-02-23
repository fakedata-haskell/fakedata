{-# LANGUAGE TemplateHaskell #-}

module Faker.Simpsons where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Simpsons
import Faker.TH


$(generateFakeField "simpsons" "characters")

$(generateFakeField "simpsons" "locations")

$(generateFakeField "simpsons" "quotes")






