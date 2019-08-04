{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Bank where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Bank
import Faker.TH

$(generateFakeField "bank" "name")

$(generateFakeField "bank" "swift_bic")
