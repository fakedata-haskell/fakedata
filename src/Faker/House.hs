{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.House where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.House
import Faker.TH

$(generateFakeField "house" "furniture")

$(generateFakeField "house" "rooms")
