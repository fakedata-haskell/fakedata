{-# LANGUAGE TemplateHaskell #-}

module Faker.TvShow.SouthPark where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.SouthPark
import Faker.TH

$(generateFakeField "southPark" "characters")

$(generateFakeField "southPark" "quotes")
