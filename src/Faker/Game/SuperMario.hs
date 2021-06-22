{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @since 1.0
module Faker.Game.SuperMario where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.SuperMario
import Faker.TH

$(generateFakeField "superMario" "characters")

$(generateFakeField "superMario" "games")

$(generateFakeField "superMario" "locations")
