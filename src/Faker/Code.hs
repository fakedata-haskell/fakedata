{-# LANGUAGE TemplateHaskell #-}

module Faker.Code where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Code
import Faker.TH

$(generateFakeField "code" "asin")
