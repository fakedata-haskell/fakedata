{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Game.StreetFighter where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.StreetFighter
import Faker.TH

$(generateFakeField "streetFighter" "characters")

$(generateFakeField "streetFighter" "stages")

$(generateFakeField "streetFighter" "quotes")

$(generateFakeField "streetFighter" "moves")
