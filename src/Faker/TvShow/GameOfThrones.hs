{-# LANGUAGE TemplateHaskell #-}

module Faker.TvShow.GameOfThrones where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.GameOfThrones
import Faker.TH

$(generateFakeField "gameOfThrones" "characters")

$(generateFakeField "gameOfThrones" "houses")

$(generateFakeField "gameOfThrones" "cities")

$(generateFakeField "gameOfThrones" "quotes")

$(generateFakeField "gameOfThrones" "dragons")
