{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Game.WorldOfWarcraft where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.WorldOfWarcraft
import Faker.TH

$(generateFakeField "worldOfWarcraft" "hero")

$(generateFakeField "worldOfWarcraft" "quotes")

-- | @since 1.0
$(generateFakeField "worldOfWarcraft" "class_names")

-- | @since 1.0
$(generateFakeField "worldOfWarcraft" "races")
