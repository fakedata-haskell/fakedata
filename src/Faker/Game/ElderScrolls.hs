{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Game.ElderScrolls where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.ElderScrolls
import Faker.TH

$(generateFakeField "elderScrolls" "race")

$(generateFakeField "elderScrolls" "creature")

$(generateFakeField "elderScrolls" "region")

$(generateFakeField "elderScrolls" "dragon")

$(generateFakeField "elderScrolls" "city")

$(generateFakeField "elderScrolls" "first_name")

$(generateFakeField "elderScrolls" "last_name")

-- | @since 1.0
$(generateFakeField "elderScrolls" "weapon")

-- | @since 1.0
$(generateFakeField "elderScrolls" "jewelry")
