{-# LANGUAGE TemplateHaskell #-}

module Faker.Hobbit where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Hobbit
import Faker.TH


$(generateFakeField "hobbit" "character")

$(generateFakeField "hobbit" "thorins_company")

$(generateFakeField "hobbit" "quote")

$(generateFakeField "hobbit" "location")






