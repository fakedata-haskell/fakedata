{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.App where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Internal
import Faker.Provider.App
import Faker.TH

$(generateFakeField "app" "name")

$(generateFakeFieldUnresolved "app" "author")

$(generateFakeFieldUnresolved "app" "version")
