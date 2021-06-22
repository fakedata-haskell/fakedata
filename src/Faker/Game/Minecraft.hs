{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Game.Minecraft where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Minecraft
import Faker.TH

$(generateFakeField "minecraft" "blocks")

$(generateFakeField "minecraft" "items")

$(generateFakeField "minecraft" "mobs")

-- | @since 1.0
$(generateFakeField "minecraft" "achievement")

-- | @since 1.0
$(generateFakeField "minecraft" "biome")

-- | @since 1.0
$(generateFakeField "minecraft" "enchantment")

-- | @since 1.0
$(generateFakeField "minecraft" "game_mode")

-- | @since 1.0
$(generateFakeField "minecraft" "status_effect")
