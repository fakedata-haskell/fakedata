{-# LANGUAGE TemplateHaskell #-}

module Faker.BreakingBad where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.BreakingBad
import Faker.TH

$(generateFakeField "breakingBad" "character")

$(generateFakeField "breakingBad" "episode")
