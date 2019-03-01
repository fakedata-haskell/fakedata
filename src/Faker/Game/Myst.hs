{-# LANGUAGE TemplateHaskell #-}

module Faker.Game.Myst where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Myst
import Faker.TH

$(generateFakeField "myst" "games")

$(generateFakeField "myst" "creatures")

$(generateFakeField "myst" "characters")

$(generateFakeField "myst" "ages")

$(generateFakeField "myst" "quotes")
