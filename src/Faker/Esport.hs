{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Esport where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Esport
import Faker.TH

$(generateFakeField "esport" "players")

$(generateFakeField "esport" "teams")

$(generateFakeField "esport" "events")

$(generateFakeField "esport" "leagues")

$(generateFakeField "esport" "games")
