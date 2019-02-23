{-# LANGUAGE TemplateHaskell #-}

module Faker.SourthPark where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.SourthPark
import Faker.TH


$(generateFakeField "sourthPark" "characters")

$(generateFakeField "sourthPark" "quotes")






