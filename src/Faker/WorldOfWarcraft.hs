{-# LANGUAGE TemplateHaskell #-}

module Faker.WorldOfWarcraft where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.WorldOfWarcraft
import Faker.TH


$(generateFakeField "worldOfWarcraft" "hero")

$(generateFakeField "worldOfWarcraft" "quotes")






