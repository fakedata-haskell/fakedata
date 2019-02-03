{-# LANGUAGE OverloadedStrings #-}

module Faker.Provider.App where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.Name (nameProvider, resolveNameText)

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

parseNameApp :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseNameApp settings = parseAppField settings "name"

parseVersionApp ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser (Unresolved a)
parseVersionApp settings = parseUnresolvedAppField settings "version"

parseAuthorApp ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser (Unresolved a)
parseAuthorApp settings = parseUnresolvedAppField settings "author"

nameAppProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
nameAppProvider settings = fetchData settings App parseNameApp

versionAppProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
versionAppProvider settings = fetchData settings App parseVersionApp

authorAppProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
authorAppProvider settings = fetchData settings App parseAuthorApp

resolveAppText :: (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveAppText settings txt = do
  let fields = resolveFields txt
  appFields <- mapM (resolveAppField settings) fields
  pure $ operateFields txt appFields

resolveAppField :: (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveAppField settings "Name.name" =
  randomUnresolvedVec settings nameProvider resolveNameText
resolveAppField settings "Company.name" = undefined
resolveAppField settings str = throwM $ InvalidField "app" str
