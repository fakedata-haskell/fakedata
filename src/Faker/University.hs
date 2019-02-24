{-# LANGUAGE TemplateHaskell #-}

module Faker.University where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.University
import Faker.TH

$(generateFakeField "university" "prefix")

$(generateFakeField "university" "suffix")

$(generateFakeFieldUnresolved "university" "name")
