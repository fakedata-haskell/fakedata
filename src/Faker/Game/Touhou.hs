{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @since 1.0
module Faker.Game.Touhou where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Touhou
import Faker.TH

$(generateFakeField "touhou" "games")

$(generateFakeField "touhou" "characters")

$(generateFakeField "touhou" "spell_cards")

$(generateFakeField "touhou" "locations")

$(generateFakeField "touhou" "songs")
