{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @since 0.2.0
module Faker.Sport.Basketball where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Basketball
import Faker.TH

$(generateFakeField "basketball" "teams")

$(generateFakeField "basketball" "players")

$(generateFakeField "basketball" "coaches")

$(generateFakeField "basketball" "positions")
