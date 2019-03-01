{-# LANGUAGE TemplateHaskell #-}

module Faker.TvShows.TwinPeaks where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.TwinPeaks
import Faker.TH

$(generateFakeField "twinPeaks" "characters")

$(generateFakeField "twinPeaks" "locations")

$(generateFakeField "twinPeaks" "quotes")
