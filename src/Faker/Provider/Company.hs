{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Company where

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
import Faker.Provider.Name (nameLastNameProvider, resolveNameText, resolveNameField)
import Faker.Provider.Address (villageProvider, communityProvider2, cityProvider2, resolveAddressField)
import Faker.Provider.TH
import Language.Haskell.TH


parseCompany :: FromJSON a => FakerSettings -> Value -> Parser a
parseCompany settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  company <- faker .: "company"
  pure company
parseCompany settings val = fail $ "expected Object, but got " <> (show val)

parseCompanyField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseCompanyField settings txt val = do
  company <- parseCompany settings val
  field <- company .:? txt .!= mempty
  pure field

parseUnresolvedCompanyField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> AesonKey
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedCompanyField settings txt val = do
  company <- parseCompany settings val
  field <- company .:? txt .!= mempty
  pure $ pure field

$(genParser "company" "suffix")

$(genProvider "company" "suffix")

$(genParser "company" "buzzwords")

companyBuzzwordsProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector (Vector Text))
companyBuzzwordsProvider settings =
  fetchData settings Company parseCompanyBuzzwords

$(genParser "company" "bs")

companyBsProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector (Vector Text))
companyBsProvider settings = fetchData settings Company parseCompanyBs

$(genParserUnresolved "company" "name")

$(genProviderUnresolved "company" "name")

$(genParser "company" "industry")

$(genProvider "company" "industry")

$(genParser "company" "profession")

$(genProvider "company" "profession")

$(genParser "company" "type")

$(genProvider "company" "type")

-- For en-ZA locale
$(genParser "company" "company_names")

$(genProvider "company" "company_names")

-- For es-mx locale
$(genParser "company" "prefix")

$(genProvider "company" "prefix")

$(genParser "company" "sic_code")

$(genProvider "company" "sic_code")

-- For ja locale

$(genParser "company" "category")

$(genProvider "company" "category")

resolveCompanyText ::
     (MonadIO m, MonadThrow m) => FakerSettings -> AesonKey -> m Text
resolveCompanyText = genericResolver' resolveCompanyField

resolveCompanyField ::
     (MonadThrow m, MonadIO m) => FakerSettings -> AesonKey -> m Text
resolveCompanyField settings "Name.last_name" =
  cachedRandomVec "name" "last_name" nameLastNameProvider settings
resolveCompanyField settings "Name.first_name" =
  resolveNameField settings "first_name"
resolveCompanyField settings field@"company_names" =
  cachedRandomVec "company" field companyCompanyNamesProvider settings
resolveCompanyField settings field@"suffix" =
  cachedRandomVec "company" field companySuffixProvider settings
resolveCompanyField settings field@"prefix" =
  cachedRandomVec "company" field companyPrefixProvider settings
resolveCompanyField settings field@"category" =
  cachedRandomVec "company" field companyCategoryProvider settings
resolveCompanyField settings "Address.village" =
  cachedRandomVec "address" "village" villageProvider settings
resolveCompanyField settings "Address.community" =
  cachedRandomVec "address" "community2" communityProvider2 settings
resolveCompanyField settings "Address.city_name" =
  resolveAddressField settings "city_name"
resolveCompanyField settings "Address.city" =
  cachedRandomVec "address" "city2" cityProvider2 settings
resolveCompanyField settings "Name.man_last_name" =
  resolveNameField settings "man_last_name"
resolveCompanyField settings "Name.male_first_name" =
  resolveNameField settings "male_first_name"
resolveCompanyField settings "Name.female_first_name" =
  resolveNameField settings "female_first_name"
resolveCompanyField settings "Name.male_last_name" =
  resolveNameField settings "male_last_name"
resolveCompanyField settings field@"product" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parseCompanyField settings field
    provider settings = fetchData settings Company parser
    in cachedRandomVec "company" field provider settings
resolveCompanyField settings str = throwM $ InvalidField "company" str
