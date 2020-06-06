{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Game.Control where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Control
import Faker.TH

$(generateFakeField "control" "character")

$(generateFakeField "control" "location")

$(generateFakeField "control" "object_of_power")

$(generateFakeField "control" "altered_item")

$(generateFakeField "control" "altered_world_event")

$(generateFakeField "control" "hiss")

$(generateFakeField "control" "the_board")

$(generateFakeField "control" "quote")



