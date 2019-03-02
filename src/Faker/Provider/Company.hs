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
import Faker.Provider.Name (lastNameProvider, resolveNameText)
import Faker.Provider.TH
import Language.Haskell.TH

parseCompany :: FromJSON a => FakerSettings -> Value -> Parser a
parseCompany settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  company <- faker .: "company"
  pure company
parseCompany settings val = fail $ "expected Object, but got " <> (show val)

parseCompanyField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseCompanyField settings txt val = do
  company <- parseCompany settings val
  field <- company .:? txt .!= mempty
  pure field

parseUnresolvedCompanyField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
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

resolveCompanyText ::
     (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveCompanyText settings txt = do
  let fields = resolveFields txt
  companyFields <- mapM (resolveCompanyField settings) fields
  pure $ operateFields txt companyFields

resolveCompanyField ::
     (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveCompanyField settings "Name.last_name" =
  randomVec settings lastNameProvider
resolveCompanyField settings "suffix" = randomVec settings companySuffixProvider
resolveCompanyField settings str = throwM $ InvalidField "company" str
