{-# LANGUAGE OverloadedStrings #-}

module Faker.Address where

import Data.Text
import Faker
import Faker.Internal
import Faker.Internal.Types (SourceData(..))
import Faker.Provider.Address

country :: Fake Text
country = Fake (cachedRandomVec Address "country" countriesProvider)

cityPrefix :: Fake Text
cityPrefix = Fake (cachedRandomVec Address "cityPrefix" cityPrefixProvider)

citySuffix :: Fake Text
citySuffix = Fake (cachedRandomVec Address "citySuffix" citySuffixProvider)

countryCode :: Fake Text
countryCode = Fake (cachedRandomVec Address "citySuffix" countryCodeProvider)

countryCodeLong :: Fake Text
countryCodeLong =
  Fake (cachedRandomVec Address "citySuffix" countryCodeLongProvider)

buildingNumber :: Fake Text
buildingNumber =
  Fake
    (cachedRandomUnresolvedVec
       Address
       "buildingNumber"
       buildingNumberProvider
       resolveAddressText)

communityPrefix :: Fake Text
communityPrefix =
  Fake (cachedRandomVec Address "communityPrefix" communityPrefixProvider)

communitySuffix :: Fake Text
communitySuffix =
  Fake (cachedRandomVec Address "communitySuffix" communitySuffixProvider)

community :: Fake Text
community =
  Fake
    (cachedRandomUnresolvedVec
       Address
       "community"
       communityProvider
       resolveAddressText)

streetSuffix :: Fake Text
streetSuffix =
  Fake (cachedRandomVec Address "streetSuffix" streetSuffixProvider)

secondaryAddress :: Fake Text
secondaryAddress =
  Fake
    (cachedRandomUnresolvedVec
       Address
       "secondaryAddress"
       secondaryAddressProvider
       resolveAddressText)

postcode :: Fake Text
postcode =
  Fake
    (cachedRandomUnresolvedVec
       Address
       "postcode"
       postcodeProvider
       resolveAddressText)

state :: Fake Text
state = Fake (cachedRandomVec Address "state" stateProvider)

stateAbbr :: Fake Text
stateAbbr = Fake (cachedRandomVec Address "stateAbbr" stateAbbrProvider)

timeZone :: Fake Text
timeZone = Fake (cachedRandomVec Address "timeZone" timeZoneProvider)

city :: Fake Text
city =
  Fake
    (cachedRandomUnresolvedVec Address "city" cityProvider resolveAddressText)

streetName :: Fake Text
streetName =
  Fake
    (cachedRandomUnresolvedVec
       Address
       "streetName"
       streetNameProvider
       resolveAddressText)

streetAddress :: Fake Text
streetAddress =
  Fake
    (cachedRandomUnresolvedVec
       Address
       "streetAddress"
       streetAddressProvider
       resolveAddressText)

fullAddress :: Fake Text
fullAddress =
  Fake
    (cachedRandomUnresolvedVec
       Address
       "fullAddress"
       fullAddressProvider
       resolveAddressText)
