{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.ProgrammingLanguage where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.ProgrammingLanguage
import Faker.TH

$(generateFakeField "programmingLauguage" "name")

$(generateFakeField "programmingLauguage" "creator")
