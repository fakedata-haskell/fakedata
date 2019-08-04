{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Address where

import Data.Text
import Faker
import Faker.Internal
import Faker.Internal.Types (SourceData(..))
import Faker.Provider.Address
import Faker.TH

country :: Fake Text
country = Fake (cachedRandomVec "address" "country" countryProvider)

$(generateFakeField2 "address" "cityPrefix")

citySuffix :: Fake Text
citySuffix = Fake (cachedRandomVec "address" "citySuffix" citySuffixProvider)

countryCode :: Fake Text
countryCode = Fake (cachedRandomVec "address" "citySuffix" countryCodeProvider)

countryCodeLong :: Fake Text
countryCodeLong =
  Fake (cachedRandomVec "address" "citySuffix" countryCodeLongProvider)

buildingNumber :: Fake Text
buildingNumber =
  Fake
    (cachedRandomUnresolvedVec
       "address"
       "buildingNumber"
       buildingNumberProvider
       resolveAddressText)

communityPrefix :: Fake Text
communityPrefix =
  Fake (cachedRandomVec "address" "communityPrefix" communityPrefixProvider)

communitySuffix :: Fake Text
communitySuffix =
  Fake (cachedRandomVec "address" "communitySuffix" communitySuffixProvider)

$(generateFakeFieldUnresolved "address" "community")

streetSuffix :: Fake Text
streetSuffix =
  Fake (cachedRandomVec "address" "streetSuffix" streetSuffixProvider)

secondaryAddress :: Fake Text
secondaryAddress =
  Fake
    (cachedRandomUnresolvedVec
       "address"
       "secondaryAddress"
       secondaryAddressProvider
       resolveAddressText)

postcode :: Fake Text
postcode =
  Fake
    (cachedRandomUnresolvedVec
       "address"
       "postcode"
       postcodeProvider
       resolveAddressText)

state :: Fake Text
state = Fake (cachedRandomVec "address" "state" stateProvider)

stateAbbr :: Fake Text
stateAbbr = Fake (cachedRandomVec "address" "stateAbbr" stateAbbrProvider)

timeZone :: Fake Text
timeZone = Fake (cachedRandomVec "address" "timeZone" timeZoneProvider)

city :: Fake Text
city =
  Fake
    (cachedRandomUnresolvedVec "address" "city" cityProvider resolveAddressText)

streetName :: Fake Text
streetName =
  Fake
    (cachedRandomUnresolvedVec
       "address"
       "streetName"
       streetNameProvider
       resolveAddressText)

streetAddress :: Fake Text
streetAddress =
  Fake
    (cachedRandomUnresolvedVec
       "address"
       "streetAddress"
       streetAddressProvider
       resolveAddressText)

fullAddress :: Fake Text
fullAddress =
  Fake
    (cachedRandomUnresolvedVec
       "address"
       "fullAddress"
       fullAddressProvider
       resolveAddressText)
