{-# LANGUAGE TemplateHaskell #-}

module Faker.JapaneseMedia.SwordArtOnline where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.SwordArtOnline
import Faker.TH

$(generateFakeField "swordArtOnline" "real_name")

$(generateFakeField "swordArtOnline" "game_name")

$(generateFakeField "swordArtOnline" "location")

$(generateFakeField "swordArtOnline" "item")
