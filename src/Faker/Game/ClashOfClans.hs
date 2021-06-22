{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @since 1.0
module Faker.Game.ClashOfClans where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.ClashOfClans
import Faker.TH

$(generateFakeField "clashOfClans" "troops")

$(generateFakeField "clashOfClans" "ranks")

$(generateFakeField "clashOfClans" "defensive_buildings")
