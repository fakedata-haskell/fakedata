{-# LANGUAGE TemplateHaskell #-}

module Faker.TvShow.BreakingBad where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.BreakingBad
import Faker.TH

$(generateFakeField "breakingBad" "character")

$(generateFakeField "breakingBad" "episode")
