{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.File where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.File
import Faker.TH

$(generateFakeField "file" "extension")

$(generateFakeField "file" "mime_type")
