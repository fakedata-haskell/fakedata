{-# LANGUAGE TemplateHaskell #-}

module Faker.PhoneNumber where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.PhoneNumber
import Faker.TH

$(generateFakeFieldUnresolved "phoneNumber" "formats")

countryCode :: Fake Text
countryCode = Fake (\settings -> randomVec settings countryCodeProvider)

cellPhoneFormat :: Fake Text
cellPhoneFormat =
  Fake
    (\settings ->
       randomUnresolvedVec
         settings
         cellPhoneFormatProvider
         resolvePhoneNumberText)
