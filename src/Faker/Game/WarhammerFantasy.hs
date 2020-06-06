{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Game.WarhammerFantasy where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.WarhammerFantasy
import Faker.TH

$(generateFakeField "warhammerFantasy" "heros")

$(generateFakeField "warhammerFantasy" "quotes")

$(generateFakeField "warhammerFantasy" "locations")

$(generateFakeField "warhammerFantasy" "factions")

$(generateFakeField "warhammerFantasy" "creatures")



