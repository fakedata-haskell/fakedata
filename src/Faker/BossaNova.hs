{-# LANGUAGE TemplateHaskell #-}

module Faker.BossaNova where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.BossaNova
import Faker.TH

$(generateFakeField "bossaNova" "artists")

$(generateFakeField "bossaNova" "songs")
