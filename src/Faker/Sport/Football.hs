{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @since 0.2.0
module Faker.Sport.Football where

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
