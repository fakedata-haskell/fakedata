{-# LANGUAGE TemplateHaskell #-}

module Faker.Coffee where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Coffee
import Faker.TH

$(generateFakeField "coffee" "country")

$(generateFakeField "coffee" "variety")

$(generateFakeField "coffee" "intensifier")

$(generateFakeField "coffee" "body")

$(generateFakeField "coffee" "descriptor")

$(generateFakeField "coffee" "notes")

$(generateFakeField "coffee" "name_1")

$(generateFakeField "coffee" "name_2")

$(generateFakeFieldUnresolved "coffee" "blend_name")
