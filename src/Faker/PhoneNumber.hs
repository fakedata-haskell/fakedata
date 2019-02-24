{-# LANGUAGE TemplateHaskell #-}

module Faker.PhoneNumber where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.PhoneNumber
import Faker.TH




$(generateFakeFieldUnresolved "phoneNumber" "formats")




