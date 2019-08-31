{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.PhoneNumber where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.PhoneNumber
import Faker.TH

$(generateFakeFieldUnresolved "phoneNumber" "formats")

countryCode :: Fake Text
countryCode =
  Fake (cachedRandomVec "phoneNumber" "countryCode" countryCodeProvider)

cellPhoneFormat :: Fake Text
cellPhoneFormat =
  Fake
    (cachedRandomUnresolvedVec
       "phoneNumber"
       "cellPhoneFormat"
       cellPhoneFormatProvider
       resolvePhoneNumberText)
