module Faker.Address where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Address

country :: Fake Text
country = Fake (\settings -> randomVec settings countriesProvider)

cityPrefix :: Fake Text
cityPrefix = Fake (\settings -> randomVec settings cityPrefixProvider)

citySuffix :: Fake Text
citySuffix = Fake (\settings -> randomVec settings citySuffixProvider)

countryCode :: Fake Text
countryCode = Fake (\settings -> randomVec settings countryCodeProvider)

countryCodeLong :: Fake Text
countryCodeLong = Fake (\settings -> randomVec settings countryCodeLongProvider)

buildingNumber :: Fake Text
buildingNumber =
  Fake
    (\settings ->
       randomUnresolvedVec settings buildingNumberProvider resolveAddressText)

communityPrefix :: Fake Text
communityPrefix = Fake (\settings -> randomVec settings communityPrefixProvider)

communitySuffix :: Fake Text
communitySuffix = Fake (\settings -> randomVec settings communitySuffixProvider)

community :: Fake Text
community =
  Fake
    (\settings ->
       randomUnresolvedVec settings communityProvider resolveAddressText)

streetSuffix :: Fake Text
streetSuffix = Fake (\settings -> randomVec settings streetSuffixProvider)

secondaryAddress :: Fake Text
secondaryAddress =
  Fake
    (\settings ->
       randomUnresolvedVec settings secondaryAddressProvider resolveAddressText)


postcode :: Fake Text
postcode = Fake (unresolvedResolver postcodeProvider resolveAddressText)

state :: Fake Text
state = Fake (resolver stateProvider)

stateAbbr :: Fake Text
stateAbbr = Fake (resolver stateAbbrProvider)

timeZone :: Fake Text
timeZone = Fake (resolver timeZoneProvider)

city :: Fake Text
city = Fake (unresolvedResolver cityProvider resolveAddressText)

streetName :: Fake Text
streetName = Fake (unresolvedResolver streetNameProvider resolveAddressText)

streetAddress :: Fake Text
streetAddress = Fake (unresolvedResolver streetAddressProvider resolveAddressText)

fullAddress :: Fake Text
fullAddress = Fake $ unresolvedResolver fullAddressProvider resolveAddressText
