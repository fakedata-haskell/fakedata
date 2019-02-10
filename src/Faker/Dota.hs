{-# LANGUAGE TemplateHaskell #-}

module Faker.Dota where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Dota
import Faker.TH

$(generateFakeField "dota" "hero")

$(generateFakeField "dota" "item")

$(generateFakeField "dota" "team")

$(generateFakeField "dota" "player")
