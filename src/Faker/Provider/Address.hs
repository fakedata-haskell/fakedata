{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Provider.Address where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.Name (nameNameProvider, resolveNameField)

parseAddress :: FromJSON a => FakerSettings -> Value -> Parser a
parseAddress settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  address <- faker .: "address"
  pure address
parseAddress settings val = fail $ "expected Object, but got " <> (show val)

parseAddressField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseAddressField settings txt val = do
  address <- parseAddress settings val
  field <- address .:? txt .!= mempty
  pure field

parseUnresolvedAddressField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedAddressField settings txt val = do
  address <- parseAddress settings val
  field <- address .:? txt .!= mempty
  pure $ pure field

parseCityPrefix :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseCityPrefix settings = parseAddressField settings "city_prefix"

-- for en-nz locale
parsePlaceNames :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parsePlaceNames settings = parseAddressField settings "place_names"

placeNamesProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
placeNamesProvider settings = fetchData settings Address parsePlaceNames

parseCitySuffix :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseCitySuffix settings = parseAddressField settings "city_suffix"

parseCountry :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseCountry settings = parseAddressField settings "country"

parseVillage :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseVillage settings = parseAddressField settings "village"

parseDefaultCountry ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseDefaultCountry settings = parseAddressField settings "default_country"

parseCountryByCode ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseCountryByCode settings = parseAddressField settings "country_by_code"

parseCountryByName ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseCountryByName settings = parseAddressField settings "country_by_name"

parseCountryCode :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseCountryCode settings = parseAddressField settings "country_code"

parseCountryCodeLong ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseCountryCodeLong settings = parseAddressField settings "country_code_long"

parseBuildingNumber ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser (Unresolved a)
parseBuildingNumber settings =
  parseUnresolvedAddressField settings "building_number"

parseMailBox ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser (Unresolved a)
parseMailBox settings = parseUnresolvedAddressField settings "mail_box"

mailBoxProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
mailBoxProvider settings = fetchData settings Address parseMailBox

parseCommunityPrefix ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseCommunityPrefix settings = parseAddressField settings "community_prefix"

parseCommunitySuffix ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseCommunitySuffix settings = parseAddressField settings "community_suffix"

parseCommunity ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser (Unresolved a)
parseCommunity settings = parseUnresolvedAddressField settings "community"

parseCommunity2 :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseCommunity2 settings = parseAddressField settings "community"

parseStreetSuffix ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseStreetSuffix settings = parseAddressField settings "street_suffix"

parseSecondaryAddress ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser (Unresolved a)
parseSecondaryAddress settings =
  parseUnresolvedAddressField settings "secondary_address"

parsePostcode ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser (Unresolved a)
parsePostcode settings = parseUnresolvedAddressField settings "postcode"

parsePostcodeByState ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser (Unresolved a)
parsePostcodeByState settings =
  parseUnresolvedAddressField settings "postcode_by_state"

parseState :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseState settings = parseAddressField settings "state"

parseStateAbbr :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseStateAbbr settings = parseAddressField settings "state_abbr"

parseTimeZone :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseTimeZone settings = parseAddressField settings "time_zone"

parseCity ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser (Unresolved a)
parseCity settings = parseUnresolvedAddressField settings "city"

parseCity2 :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseCity2 settings = parseAddressField settings "city"

parseStreetName ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser (Unresolved a)
parseStreetName settings = parseUnresolvedAddressField settings "street_name"

parseStreetAddress ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser (Unresolved a)
parseStreetAddress settings =
  parseUnresolvedAddressField settings "street_address"

parseFullAddress ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser (Unresolved a)
parseFullAddress settings = parseUnresolvedAddressField settings "full_address"

countryProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
countryProvider settings = fetchData settings Address parseCountry

defaultCountryProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
defaultCountryProvider settings = fetchData settings Address parseDefaultCountry

villageProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
villageProvider settings = fetchData settings Address parseVillage

addressCityPrefixProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
addressCityPrefixProvider settings = fetchData settings Address parseCityPrefix

citySuffixProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
citySuffixProvider settings = fetchData settings Address parseCitySuffix

countryByCodeProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Map Text Text)
countryByCodeProvider settings = fetchData settings Address parseCountryByCode

countryByNameProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Map Text Text)
countryByNameProvider settings = fetchData settings Address parseCountryByName

countryCodeProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
countryCodeProvider settings = fetchData settings Address parseCountryCode

countryCodeLongProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
countryCodeLongProvider settings =
  fetchData settings Address parseCountryCodeLong

buildingNumberProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
buildingNumberProvider settings = fetchData settings Address parseBuildingNumber

communityPrefixProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
communityPrefixProvider settings =
  fetchData settings Address parseCommunityPrefix

communitySuffixProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
communitySuffixProvider settings =
  fetchData settings Address parseCommunitySuffix

communityProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
communityProvider settings = fetchData settings Address parseCommunity

-- For hy locale
communityProvider2 ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
communityProvider2 settings = fetchData settings Address parseCommunity2

addressCommunityProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
addressCommunityProvider = communityProvider

streetSuffixProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
streetSuffixProvider settings = fetchData settings Address parseStreetSuffix

secondaryAddressProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
secondaryAddressProvider settings =
  fetchData settings Address parseSecondaryAddress

postcodeRegexProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m Text
postcodeRegexProvider settings = do
  let parser :: FakerSettings -> Value -> Parser Text
      parser = \settings -> parseAddressField settings "postcode"
  val <- fetchDataSingle settings Address parser
  pure $ V.head val

postcodeProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
postcodeProvider settings = fetchData settings Address parsePostcode

-- todo: write test case for this in resolver
postcodeByStateProvider ::
     (MonadThrow m, MonadIO m)
  => FakerSettings
  -> m (Unresolved (Map Text Text))
postcodeByStateProvider settings =
  fetchData settings Address parsePostcodeByState

stateProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
stateProvider settings = fetchData settings Address parseState

stateAbbrProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
stateAbbrProvider settings = fetchData settings Address parseStateAbbr

timeZoneProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
timeZoneProvider settings = fetchData settings Address parseTimeZone

cityProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
cityProvider settings = fetchData settings Address parseCity

cityProvider2 :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
cityProvider2 settings = fetchData settings Address parseCity2

streetNameProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
streetNameProvider settings = fetchData settings Address parseStreetName

streetAddressProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
streetAddressProvider settings = fetchData settings Address parseStreetAddress

fullAddressProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
fullAddressProvider settings = fetchData settings Address parseFullAddress

-- > resolveCommunityText "#{community_prefix} #{community_suffix}"
resolveAddressText ::
     (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveAddressText settings txt = do
  communityFields :: [Text] <-
    mapM
      (\(seed, field) ->
         resolveAddressField (modifyRandomGen settings seed) field)
      (zip [1 ..] fields)
  pure $ operateFields txt communityFields
  where
    fields = resolveFields txt

resolveAddressField ::
     (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveAddressField settings field@"community_suffix" =
  cachedRandomVec "address" field communitySuffixProvider settings
resolveAddressField settings field@"community_prefix" =
  cachedRandomVec "address" field communityPrefixProvider settings
resolveAddressField settings field@"city_prefix" =
  cachedRandomVec "address" field addressCityPrefixProvider settings
resolveAddressField settings field@"first_name" =
  resolveNameField settings "first_name"
resolveAddressField settings field@"last_name" =
  resolveNameField settings "last_name"
resolveAddressField settings field@"Name.first_name" =
  resolveNameField settings "first_name"
resolveAddressField settings field@"Name.name" =
  cachedRandomUnresolvedVec
    "address"
    "name"
    nameNameProvider
    resolveAddressText
    settings
resolveAddressField settings field@"city_suffix" =
  cachedRandomVec "address" field citySuffixProvider settings
resolveAddressField settings field@"Name.last_name" =
  resolveNameField settings "last_name"
resolveAddressField settings field@"Name.surname" =
  resolveNameField settings "surname"
resolveAddressField settings field@"surname" =
  resolveNameField settings "surname"
resolveAddressField settings field@"street_suffix" =
  cachedRandomVec "address" field streetSuffixProvider settings
resolveAddressField settings field@"building_number" =
  cachedRandomUnresolvedVec
    "address"
    field
    buildingNumberProvider
    resolveAddressText
    settings
resolveAddressField settings field@"street_name" =
  cachedRandomUnresolvedVec
    "address"
    field
    streetNameProvider
    resolveAddressText
    settings
resolveAddressField settings field@"street_address" =
  cachedRandomUnresolvedVec
    "address"
    field
    streetAddressProvider
    resolveAddressText
    settings
resolveAddressField settings field@"city" =
  cachedRandomUnresolvedVec
    "address"
    field
    cityProvider
    resolveAddressText
    settings
resolveAddressField settings field@"state_abbr" =
  cachedRandomVec "address" field stateAbbrProvider settings
resolveAddressField settings field@"zip_code" =
  cachedRandomUnresolvedVec
    "address"
    field
    postcodeProvider
    resolveAddressText
    settings
resolveAddressField settings field@"secondary_address" =
  cachedRandomUnresolvedVec
    "address"
    field
    secondaryAddressProvider
    resolveAddressText
    settings
resolveAddressField settings field@"city_name" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"city_names" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"city_root" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"cities" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"municipality" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"street_title" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"state" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"streets" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"country" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"street_prefix" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"street_root" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"common_street_suffix" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"village_prefix" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"village" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"postcode" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
  in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"street" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"masculine_street_prefix" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"feminine_street_prefix" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"masculine_street_title" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"feminine_street_title" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"Address.city_name" =
  resolveAddressField settings "city_name"
resolveAddressField settings field@"Address.street_title" =
  resolveAddressField settings "street_title"
resolveAddressField settings field@"default_country" =
  cachedRandomVec "address" field defaultCountryProvider settings
resolveAddressField settings field@"mail_box" =
  cachedRandomUnresolvedVec
    "address"
    field
    mailBoxProvider
    resolveAddressText
    settings
resolveAddressField settings field@"community" =
  cachedRandomUnresolvedVec
    "address"
    field
    communityProvider
    resolveAddressText
    settings
resolveAddressField settings field@"place_names" =
  cachedRandomVec "address" field placeNamesProvider settings
resolveAddressField settings field@"landscape_elements" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"colonialism" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings field@"the" =
  let parser :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
      parser settings = parseAddressField settings field
      provider settings = fetchData settings Address parser
   in cachedRandomVec "address" field provider settings
resolveAddressField settings str = throwM $ InvalidField "address" str
