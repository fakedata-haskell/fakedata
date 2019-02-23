{-# LANGUAGE TemplateHaskell #-}

module Faker.ProgrammingLauguage where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.ProgrammingLauguage
import Faker.TH


$(generateFakeField "programmingLauguage" "name")

$(generateFakeField "programmingLauguage" "creator")






