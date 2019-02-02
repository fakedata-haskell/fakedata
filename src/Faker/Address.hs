{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE BangPatterns#-}
{-#LANGUAGE ScopedTypeVariables#-}

module Faker.Address where

import Data.Yaml
import Faker
import Config
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import Control.Monad.Catch
import Data.Text
import System.Directory (doesFileExist)
import System.FilePath
import Control.Monad.IO.Class
import qualified Data.Text as T
import System.Random
import Debug.Trace
import Faker.Name (resolveNameField)
import Faker.Combinators

parseAddress :: FromJSON a => FakerSettings ->  Value -> Parser a
parseAddress settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  address <- faker .: "address"
  pure address
parseAddress settings val = fail $ "expected Object, but got " <> (show val)

parseAddressField :: FromJSON a => FakerSettings ->  Text -> Value -> Parser a
parseAddressField settings txt val = do
  address <- parseAddress settings val
  field <- address .: txt
  pure field

parseUnresolvedAddressField :: FromJSON a => FakerSettings ->  Text -> Value -> Parser (Unresolved a)
parseUnresolvedAddressField settings txt val = do
  address <- parseAddress settings val
  field <- address .: txt
  pure $ pure field

parseCityPrefix :: FromJSON a => FakerSettings ->  Value -> Parser a
parseCityPrefix settings val = do
  address <- parseAddress settings val
  cityPrefix <- address .: "city_prefix"
  pure cityPrefix

parseCitySuffix :: FromJSON a => FakerSettings ->  Value -> Parser a
parseCitySuffix settings val = do
  address <- parseAddress settings val
  citySuffix <- address .: "city_suffix"
  pure citySuffix

parseCountry :: (FromJSON a, Monoid a) => FakerSettings ->  Value -> Parser a
parseCountry settings val = do
  address <- parseAddress settings val
  country <- address .:? "country" .!= mempty
  pure country

parseCountryByCode :: FromJSON a => FakerSettings ->  Value -> Parser a
parseCountryByCode settings = parseAddressField settings "country_by_code"

parseCountryByName :: FromJSON a => FakerSettings ->  Value -> Parser a
parseCountryByName settings = parseAddressField settings  "country_by_name"

parseCountryCode :: FromJSON a => FakerSettings ->  Value -> Parser a
parseCountryCode settings = parseAddressField settings "country_code"

parseCountryCodeLong :: FromJSON a => FakerSettings ->  Value -> Parser a
parseCountryCodeLong settings = parseAddressField settings "country_code_long"

parseBuildingNumber :: FromJSON a => FakerSettings ->  Value -> Parser (Unresolved a)
parseBuildingNumber settings = parseUnresolvedAddressField settings "building_number"

parseCommunityPrefix :: FromJSON a => FakerSettings ->  Value -> Parser a
parseCommunityPrefix settings val = do
  address <- parseAddress settings val
  community_prefix <- address .: "community_prefix"
  pure community_prefix

parseCommunitySuffix :: FromJSON a => FakerSettings ->  Value -> Parser a
parseCommunitySuffix settings val = do
  address <- parseAddress settings val
  community_suffix <- address .: "community_suffix"
  pure community_suffix

parseCommunity :: FromJSON a => FakerSettings ->  Value -> Parser (Unresolved a)
parseCommunity settings val = do
  address <- parseAddress settings val
  community <- address .: "community"
  pure $ pure community

parseStreetSuffix :: FromJSON a => FakerSettings ->  Value -> Parser a
parseStreetSuffix settings = parseAddressField settings "street_suffix"

parseSecondaryAddress :: FromJSON a => FakerSettings ->  Value -> Parser (Unresolved a)
parseSecondaryAddress settings = parseUnresolvedAddressField settings  "secondary_address"

parsePostcode :: FromJSON a => FakerSettings ->  Value -> Parser (Unresolved a)
parsePostcode settings = parseUnresolvedAddressField settings  "postcode"

parsePostcodeByState :: FromJSON a => FakerSettings ->  Value -> Parser (Unresolved a)
parsePostcodeByState settings = parseUnresolvedAddressField settings  "postcode_by_state"

parseState :: FromJSON a => FakerSettings ->  Value -> Parser a
parseState settings = parseAddressField settings "state"

parseStateAbbr :: FromJSON a => FakerSettings ->  Value -> Parser a
parseStateAbbr settings = parseAddressField settings "state_abbr"

parseTimeZone :: FromJSON a => FakerSettings ->  Value -> Parser a
parseTimeZone settings = parseAddressField settings "time_zone"

parseCity :: FromJSON a => FakerSettings ->  Value -> Parser (Unresolved a)
parseCity settings = parseUnresolvedAddressField settings  "city"

parseStreetName :: FromJSON a => FakerSettings ->  Value -> Parser (Unresolved a)
parseStreetName settings = parseUnresolvedAddressField settings  "street_name"

parseStreetAddress :: FromJSON a => FakerSettings ->  Value -> Parser (Unresolved a)
parseStreetAddress settings = parseUnresolvedAddressField settings  "street_address"

parseFullAddress :: FromJSON a => FakerSettings ->  Value -> Parser (Unresolved a)
parseFullAddress settings = parseUnresolvedAddressField settings "full_address"

countriesProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
countriesProvider settings = fetchData settings Address parseCountry

cityPrefixProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
cityPrefixProvider settings = fetchData settings Address parseCityPrefix

citySuffixProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
citySuffixProvider settings = fetchData settings Address parseCitySuffix

countryByCodeProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Map Text Text)
countryByCodeProvider settings = fetchData settings Address parseCountryByCode

countryByNameProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Map Text Text)
countryByNameProvider settings = fetchData settings Address parseCountryByName

countryCodeProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
countryCodeProvider settings = fetchData settings Address parseCountryCode

countryCodeLongProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
countryCodeLongProvider settings = fetchData settings Address parseCountryCodeLong

buildingNumberProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
buildingNumberProvider settings = fetchData settings Address parseBuildingNumber

communityPrefixProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
communityPrefixProvider settings = fetchData settings Address parseCommunityPrefix

communitySuffixProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
communitySuffixProvider settings = fetchData settings Address parseCommunitySuffix

communityProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
communityProvider settings = fetchData settings Address parseCommunity

streetSuffixProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
streetSuffixProvider settings = fetchData settings Address parseStreetSuffix

secondaryAddressProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
secondaryAddressProvider settings = fetchData settings Address parseSecondaryAddress

postcodeProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
postcodeProvider settings = fetchData settings Address parsePostcode

-- todo: write test case for this in resolver
postcodeByStateProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Map Text Text))
postcodeByStateProvider settings = fetchData settings Address parsePostcodeByState

stateProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
stateProvider settings = fetchData settings Address parseState

stateAbbrProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
stateAbbrProvider settings = fetchData settings Address parseStateAbbr

timeZoneProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
timeZoneProvider settings = fetchData settings Address parseTimeZone

cityProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
cityProvider settings = fetchData settings Address parseCity

streetNameProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
streetNameProvider settings = fetchData settings Address parseStreetName

streetAddressProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
streetAddressProvider settings = fetchData settings Address parseStreetAddress

fullAddressProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
fullAddressProvider settings = fetchData settings Address parseFullAddress

-- > resolveCommunityText "#{community_prefix} #{community_suffix}"
resolveAddressText :: (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveAddressText settings txt = do
    communityFields :: [Text] <- mapM (resolveAddressField settings) fields
    pure $ operateFields txt communityFields
    where
      fields = resolveFields txt

resolveAddressField :: (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveAddressField settings "community_suffix" = randomVec settings communitySuffixProvider
resolveAddressField settings "community_prefix" = randomVec settings communityPrefixProvider
resolveAddressField settings "city_prefix" = randomVec settings cityPrefixProvider
resolveAddressField settings "Name.first_name" = resolveNameField settings "first_name"
resolveAddressField settings "city_suffix" = randomVec settings citySuffixProvider
resolveAddressField settings "Name.last_name" = resolveNameField settings "last_name"
resolveAddressField settings "street_suffix" = randomVec settings streetSuffixProvider
resolveAddressField settings "building_number" = randomUnresolvedVec settings buildingNumberProvider resolveAddressText
resolveAddressField settings "street_name" = randomUnresolvedVec settings streetNameProvider resolveAddressText
resolveAddressField settings "street_address" = randomUnresolvedVec settings streetAddressProvider resolveAddressText
resolveAddressField settings "city" = randomUnresolvedVec settings cityProvider resolveAddressText
resolveAddressField settings "state_abbr" = randomVec settings stateAbbrProvider
resolveAddressField settings "zip_code" = randomUnresolvedVec settings postcodeProvider resolveAddressText
resolveAddressField settings "secondary_address" = randomUnresolvedVec settings secondaryAddressProvider resolveAddressText
resolveAddressField settings str = throwM $ InvalidField "address" str

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
buildingNumber = Fake (\settings -> randomUnresolvedVec settings buildingNumberProvider resolveAddressText)

communityPrefix :: Fake Text
communityPrefix = Fake (\settings -> randomVec settings communityPrefixProvider)

communitySuffix :: Fake Text
communitySuffix = Fake (\settings -> randomVec settings communitySuffixProvider)

community :: Fake Text
community = Fake (\settings -> randomUnresolvedVec settings communityProvider resolveAddressText)

streetSuffix :: Fake Text
streetSuffix = Fake (\settings -> randomVec settings streetSuffixProvider)

secondaryAddress :: Fake Text
secondaryAddress = Fake (\settings -> randomUnresolvedVec settings secondaryAddressProvider resolveAddressText)

resolver :: (MonadThrow m, MonadIO m) => (FakerSettings -> m (Vector Text)) -> FakerSettings -> m Text
resolver provider = \settings -> randomVec settings provider

unresolvedResolver :: (MonadThrow m, MonadIO m) => (FakerSettings -> m (Unresolved (Vector Text))) -> FakerSettings -> m Text
unresolvedResolver provider = \settings -> randomUnresolvedVec settings provider resolveAddressText

postcode :: Fake Text
postcode = Fake (unresolvedResolver postcodeProvider)

state :: Fake Text
state = Fake (resolver stateProvider)

stateAbbr :: Fake Text
stateAbbr = Fake (resolver stateAbbrProvider)

timeZone :: Fake Text
timeZone = Fake (resolver timeZoneProvider)

city :: Fake Text
city = Fake (unresolvedResolver cityProvider)

streetName :: Fake Text
streetName = Fake (unresolvedResolver streetNameProvider)

streetAddress :: Fake Text
streetAddress = Fake (unresolvedResolver streetAddressProvider)

fullAddress :: Fake Text
fullAddress = Fake $ unresolvedResolver fullAddressProvider
