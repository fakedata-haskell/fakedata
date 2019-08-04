{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Business where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Business
import Faker.TH

$(generateFakeField "business" "credit_card_numbers")

$(generateFakeField "business" "credit_card_types")
