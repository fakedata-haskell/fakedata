{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Name where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Name
import Faker.TH

$(generateFakeField "name" "male_first_name")

$(generateFakeField "name" "female_first_name")

$(generateFakeField "name" "prefix")

$(generateFakeField "name" "suffix")

$(generateFakeField "name" "last_name")

$(generateFakeFieldUnresolved "name" "name")

$(generateFakeFieldUnresolved "name" "name_with_middle")

$(generateFakeFieldUnresolved "name" "first_name")
