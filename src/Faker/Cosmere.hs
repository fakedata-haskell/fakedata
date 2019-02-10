{-# LANGUAGE TemplateHaskell #-}

module Faker.Cosmere where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Cosmere
import Faker.TH

$(generateFakeField "cosmere" "aons")

$(generateFakeField "cosmere" "shard_worlds")

$(generateFakeField "cosmere" "shards")

$(generateFakeField "cosmere" "surges")

$(generateFakeField "cosmere" "knights_radiant")

$(generateFakeField "cosmere" "metals")

$(generateFakeField "cosmere" "allomancers")

$(generateFakeField "cosmere" "feruchemists")

$(generateFakeField "cosmere" "heralds")

$(generateFakeField "cosmere" "sprens")
