{-# LANGUAGE TemplateHaskell #-}

module Faker.CryptoCoin where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.CryptoCoin
import Faker.TH

$(generateFakeField "cryptoCoin" "coin")
