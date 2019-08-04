{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Team where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Team
import Faker.TH

$(generateFakeField "team" "creature")

$(generateFakeField "team" "sport")

$(generateFakeField "team" "mascot")

$(generateFakeFieldUnresolved "team" "name")
