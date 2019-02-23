{-# LANGUAGE TemplateHaskell #-}

module Faker.FunnyName where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.FunnyName
import Faker.TH


$(generateFakeField "funnyName" "name")






