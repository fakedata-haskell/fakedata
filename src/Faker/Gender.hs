{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Gender where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Gender
import Faker.TH

$(generateFakeField "gender" "types")

$(generateFakeField "gender" "binary_types")
