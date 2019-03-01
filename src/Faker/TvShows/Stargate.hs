{-# LANGUAGE TemplateHaskell #-}

module Faker.TvShows.Stargate where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Stargate
import Faker.TH

$(generateFakeField "stargate" "characters")

$(generateFakeField "stargate" "planets")

$(generateFakeField "stargate" "quotes")
