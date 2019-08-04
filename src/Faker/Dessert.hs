{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Dessert where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Dessert
import Faker.TH

$(generateFakeField "dessert" "variety")

$(generateFakeField "dessert" "topping")

$(generateFakeField "dessert" "flavor")
