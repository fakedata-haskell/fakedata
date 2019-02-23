{-# LANGUAGE TemplateHaskell #-}

module Faker.Nation where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Nation
import Faker.TH


$(generateFakeField "nation" "nationality")

$(generateFakeField "nation" "language")

$(generateFakeField "nation" "capital_city")






