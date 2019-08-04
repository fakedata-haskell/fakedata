{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.TvShow.TwinPeaks where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.TwinPeaks
import Faker.TH

$(generateFakeField "twinPeaks" "characters")

$(generateFakeField "twinPeaks" "locations")

$(generateFakeField "twinPeaks" "quotes")
