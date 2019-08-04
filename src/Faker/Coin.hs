{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Coin where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Coin
import Faker.TH

$(generateFakeField "coin" "flip")
