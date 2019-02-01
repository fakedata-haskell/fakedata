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

parseAddress :: FromJSON a => Value -> Parser a
parseAddress (Object obj) = do
  en <- obj .: "en"
  faker <- en .: "faker"
  address <- faker .: "address"
  pure address
parseAddress val = fail $ "expected Object, but got " <> (show val)

parseAddressField :: FromJSON a => Text -> Value -> Parser a
parseAddressField txt val = do
  address <- parseAddress val
  field <- address .: txt
  pure field

parseUnresolvedAddressField :: FromJSON a => Text -> Value -> Parser (Unresolved a)
parseUnresolvedAddressField txt val = do
  address <- parseAddress val
  field <- address .: txt
  pure $ pure field

parseCityPrefix :: FromJSON a => Value -> Parser a
parseCityPrefix val = do
  address <- parseAddress val
  cityPrefix <- address .: "city_prefix"
  pure cityPrefix

parseCitySuffix :: FromJSON a => Value -> Parser a
parseCitySuffix val = do
  address <- parseAddress val
  citySuffix <- address .: "city_suffix"
  pure citySuffix

parseCountry :: FromJSON a => Value -> Parser a
parseCountry val = do
  address <- parseAddress val
  country <- address .: "country"
  pure country

parseCountryByCode :: FromJSON a => Value -> Parser a
parseCountryByCode = parseAddressField "country_by_code"

parseCountryByName :: FromJSON a => Value -> Parser a
parseCountryByName = parseAddressField "country_by_name"

parseCountryCode :: FromJSON a => Value -> Parser a
parseCountryCode = parseAddressField "country_code"

parseCountryCodeLong :: FromJSON a => Value -> Parser a
parseCountryCodeLong = parseAddressField "country_code_long"

parseBuildingNumber :: FromJSON a => Value -> Parser (Unresolved a)
parseBuildingNumber = parseUnresolvedAddressField "building_number"

parseCommunityPrefix :: Value -> Parser (Vector Text)
parseCommunityPrefix val = do
  address <- parseAddress val
  community_prefix <- address .: "community_prefix"
  pure community_prefix

parseCommunitySuffix :: Value -> Parser (Vector Text)
parseCommunitySuffix val = do
  address <- parseAddress val
  community_suffix <- address .: "community_suffix"
  pure community_suffix

parseCommunity :: Value -> Parser (Unresolved (Vector Text))
parseCommunity val = do
  address <- parseAddress val
  community <- address .: "community"
  pure $ pure community

parseStreetSuffix :: Value -> Parser (Vector Text)
parseStreetSuffix = parseAddressField "street_suffix"

parseSecondaryAddress :: FromJSON a => Value -> Parser (Unresolved a)
parseSecondaryAddress = parseUnresolvedAddressField "secondary_address"

parsePostcode :: FromJSON a => Value -> Parser (Unresolved a)
parsePostcode = parseUnresolvedAddressField "postcode"

parsePostcodeByState :: FromJSON a => Value -> Parser (Unresolved a)
parsePostcodeByState = parseUnresolvedAddressField "postcode_by_state"

parseState :: FromJSON a => Value -> Parser a
parseState = parseAddressField "state"

parseStateAbbr :: FromJSON a => Value -> Parser a
parseStateAbbr = parseAddressField "state_abbr"

parseTimeZone :: FromJSON a => Value -> Parser a
parseTimeZone = parseAddressField "time_zone"

parseCity :: FromJSON a => Value -> Parser (Unresolved a)
parseCity = parseUnresolvedAddressField "city"

parseStreetName :: FromJSON a => Value -> Parser (Unresolved a)
parseStreetName = parseUnresolvedAddressField "street_name"

parseStreetAddress :: FromJSON a => Value -> Parser (Unresolved a)
parseStreetAddress = parseUnresolvedAddressField "street_address"

parseFullAddress :: FromJSON a => Value -> Parser (Unresolved a)
parseFullAddress = parseUnresolvedAddressField "full_address"

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

fullAddressProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Map Text Text))
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
resolveAddressField settings "Name.first_name" = undefined
resolveAddressField settings "city_suffix" = randomVec settings citySuffixProvider
resolveAddressField settings "Name.last_name" = undefined
resolveAddressField settings "street_suffix" = randomVec settings streetSuffixProvider
resolveAddressField settings "building_number" = randomUnresolvedVec settings buildingNumberProvider resolveAddressField
resolveAddressField settings "street_name" = randomUnresolvedVec settings streetNameProvider resolveAddressField
resolveAddressField settings "street_address" = randomUnresolvedVec settings streetAddressProvider resolveAddressField
resolveAddressField settings "city" = randomUnresolvedVec settings cityProvider resolveAddressField
resolveAddressField settings "state_abbr" = randomVec settings stateAbbrProvider
resolveAddressField settings "zip_code" = randomUnresolvedVec settings postcodeProvider resolveAddressField
resolveAddressField settings "secondary_address" = randomUnresolvedVec settings secondaryAddressProvider resolveAddressField
resolveAddressField settings str = throwM $ InvalidField "address" str





-- https://hackage.haskell.org/package/fake-0.1.1.1/docs/Fake.html
-- FGen type has to be modified to take a Config { cfLocale :: String, cfStdGen :: Gen}
