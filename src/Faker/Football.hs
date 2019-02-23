{-# LANGUAGE TemplateHaskell #-}

module Faker.Football where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Football
import Faker.TH

$(generateFakeField "football" "teams")

$(generateFakeField "football" "players")

$(generateFakeField "football" "coaches")

$(generateFakeField "football" "competitions")

$(generateFakeField "football" "positions")
