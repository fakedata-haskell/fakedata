{-# LANGUAGE TemplateHaskell #-}

module Faker.App where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.App
import Faker.TH

$(generateFakeField "app" "name")

$(generateFakeFieldUnresolved "app" "author")

$(generateFakeFieldUnresolved "app" "version")
