{-# LANGUAGE TemplateHaskell #-}

module Faker.TvShows.TheThickOfIt where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.TheThickOfIt
import Faker.TH

$(generateFakeField "theThickOfIt" "characters")

$(generateFakeField "theThickOfIt" "positions")

$(generateFakeField "theThickOfIt" "departments")
