{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Demographic where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Demographic
import Faker.TH

$(generateFakeField "demographic" "race")

$(generateFakeField "demographic" "sex")

$(generateFakeField "demographic" "demonym")

$(generateFakeField "demographic" "educational_attainment")

$(generateFakeField "demographic" "marital_status")
