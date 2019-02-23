{-# LANGUAGE TemplateHaskell #-}

module Faker.Marketing where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Marketing
import Faker.TH


$(generateFakeField "marketing" "buzzwords")






