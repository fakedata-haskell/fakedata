{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Currency where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Currency
import Faker.TH

$(generateFakeField "currency" "name")

$(generateFakeField "currency" "code")

$(generateFakeField "currency" "symbol")
