{-# LANGUAGE TemplateHaskell #-}

module Faker.Company where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Company
import Faker.TH

$(generateFakeField "company" "suffix")

$(generateFakeField "company" "buzzwords")

$(generateFakeField "company" "bs")

$(generateFakeFieldUnresolved "company" "name")

$(generateFakeField "company" "industry")

$(generateFakeField "company" "profession")

$(generateFakeField "company" "type")
