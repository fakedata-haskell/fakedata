{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Minecraft where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Minecraft
import Faker.TH

$(generateFakeField "minecraft" "blocks")

$(generateFakeField "minecraft" "items")

$(generateFakeField "minecraft" "mobs")
