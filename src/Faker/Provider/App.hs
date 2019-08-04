{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.App where

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
import Faker.Provider.Company (companyNameProvider, resolveCompanyText)
import Faker.Provider.Name (nameProvider, resolveNameText)
import Faker.Provider.TH
import Language.Haskell.TH

parseApp :: FromJSON a => FakerSettings -> Value -> Parser a
parseApp settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  app <- faker .: "app"
  pure app
parseApp settings val = fail $ "expected Object, but got " <> (show val)

parseAppField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseAppField settings txt val = do
  app <- parseApp settings val
  field <- app .:? txt .!= mempty
  pure field

parseUnresolvedAppField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedAppField settings txt val = do
  app <- parseApp settings val
  field <- app .:? txt .!= mempty
  pure $ pure field

$(genAppParser "name")

$(genAppProvider "name")

$(genAppParserUnresolved "version")

$(genAppParserUnresolved "author")

$(genAppProviderUnresolved "version")

$(genAppProviderUnresolved "author")

resolveAppText :: (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveAppText settings txt = do
  let fields = resolveFields txt
  appFields <- mapM (resolveAppField settings) fields
  pure $ operateFields txt appFields

resolveAppField :: (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveAppField settings "Name.name" =
  randomUnresolvedVec settings nameProvider resolveNameText
resolveAppField settings "Company.name" =
  randomUnresolvedVec settings companyNameProvider resolveCompanyText
resolveAppField settings str = throwM $ InvalidField "app" str
