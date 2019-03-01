{-# LANGUAGE TemplateHaskell #-}

module Faker.TvShows.Rupaul where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Rupaul
import Faker.TH

$(generateFakeField "rupaul" "queens")

$(generateFakeField "rupaul" "quotes")
