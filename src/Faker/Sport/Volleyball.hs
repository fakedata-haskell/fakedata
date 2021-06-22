{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @since 1.0
module Faker.Sport.Volleyball where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Volleyball
import Faker.TH

$(generateFakeField "volleyball" "team")

$(generateFakeField "volleyball" "player")

$(generateFakeField "volleyball" "coach")

$(generateFakeField "volleyball" "position")

$(generateFakeField "volleyball" "formation")
