{-# LANGUAGE TemplateHaskell #-}

module Faker.Music.Music where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Music
import Faker.TH

$(generateFakeField "music" "instruments")

$(generateFakeField "music" "bands")

$(generateFakeField "music" "albums")

$(generateFakeField "music" "genres")
