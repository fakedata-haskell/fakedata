{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.PhoneNumber where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parsePhoneNumber :: FromJSON a => FakerSettings -> Value -> Parser a
parsePhoneNumber settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  phoneNumber <- faker .: "phone_number"
  pure phoneNumber
parsePhoneNumber settings val = fail $ "expected Object, but got " <> (show val)

parsePhoneNumberField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parsePhoneNumberField settings txt val = do
  phoneNumber <- parsePhoneNumber settings val
  field <- phoneNumber .:? txt .!= mempty
  pure field

parsePhoneNumberFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parsePhoneNumberFields settings txts val = do
  phoneNumber <- parsePhoneNumber settings val
  helper phoneNumber txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

parseCellPhone :: FromJSON a => FakerSettings -> Value -> Parser a
parseCellPhone settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  phoneNumber <- faker .: "cell_phone"
  pure phoneNumber
parseCellPhone settings val = fail $ "expected Object, but got " <> (show val)

parseCellPhoneField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseCellPhoneField settings txt val = do
  phoneNumber <- parsePhoneNumber settings val
  field <- phoneNumber .:? txt .!= mempty
  pure field

parseUnresolvedCellPhoneField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedCellPhoneField settings txt val = do
  cellphone <- parseCellPhone settings val
  field <- cellphone .:? txt .!= mempty
  pure $ pure field

parseUnresolvedPhoneNumberField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedPhoneNumberField settings txt val = do
  phoneNumber <- parsePhoneNumber settings val
  field <- phoneNumber .:? txt .!= mempty
  pure $ pure field

$(genParserUnresolved "phoneNumber" "formats")

$(genProviderUnresolved "phoneNumber" "formats")

$(genParserUnresolved "cellPhone" "formats")

$(genParserUnresolved "cellPhone" "subscriber_number")

cellPhoneFormatProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
cellPhoneFormatProvider settings =
  fetchData settings PhoneNumber parseCellPhoneFormatsUnresolved

-- $(genProvider "cellPhone" "formats")
parseCountryCode :: FromJSON a => FakerSettings -> Value -> Parser a
parseCountryCode settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  countryCode <- faker .: "country_code"
  pure countryCode
parseCountryCode settings val = fail $ "expected Object, but got " <> (show val)

countryCodeProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
countryCodeProvider settings = fetchData settings PhoneNumber parseCountryCode

resolvePhoneNumberText ::
     (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolvePhoneNumberText settings txt = do
  let fields = resolveFields txt
  phoneNumberFields <- mapM (resolvePhoneNumberField settings) fields
  pure $ operateFields txt phoneNumberFields

resolvePhoneNumberField ::
     (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolvePhoneNumberField settings field@"PhoneNumber.area_code" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parsePhoneNumberField settings "area_code"
    provider settings = fetchData settings PhoneNumber parser
    in cachedRandomVec "phoneNumber" field provider settings
resolvePhoneNumberField settings field@"PhoneNumber.exchange_code" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parsePhoneNumberField settings "exchange_code"
    provider settings = fetchData settings PhoneNumber parser
    in cachedRandomVec "phoneNumber" field provider settings
resolvePhoneNumberField settings field@"PhoneNumber.subscriber_number" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parsePhoneNumberField settings "subscriber_number"
    provider settings = fetchData settings PhoneNumber parser
    in cachedRandomVec "phoneNumber" field provider settings
resolvePhoneNumberField settings field@"PhoneNumber.extension" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parsePhoneNumberField settings "extension"
    provider settings = fetchData settings PhoneNumber parser
    in cachedRandomVec "phoneNumber" field provider settings
resolvePhoneNumberField settings field@"area_code" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parsePhoneNumberField settings "area_code"
    provider settings = fetchData settings PhoneNumber parser
    in cachedRandomVec "phoneNumber" field provider settings
resolvePhoneNumberField settings field@"subscriber_number" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parsePhoneNumberField settings "subscriber_number"
    provider settings = fetchData settings PhoneNumber parser
    in cachedRandomVec "phoneNumber" field provider settings
resolvePhoneNumberField settings field@"country_code" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parsePhoneNumberField settings field
    provider settings = fetchData settings PhoneNumber parser
    in cachedRandomVec "phoneNumber" field provider settings
resolvePhoneNumberField settings str = throwM $ InvalidField "phoneNumber" str
