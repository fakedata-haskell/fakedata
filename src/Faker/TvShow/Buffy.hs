{-# LANGUAGE TemplateHaskell #-}

module Faker.TvShow.Buffy where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Buffy
import Faker.TH

$(generateFakeField "buffy" "characters")

$(generateFakeField "buffy" "quotes")

$(generateFakeField "buffy" "celebrities")

$(generateFakeField "buffy" "big_bads")

$(generateFakeField "buffy" "episodes")
