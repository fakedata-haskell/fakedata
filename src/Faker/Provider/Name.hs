{-# LANGUAGE OverloadedStrings #-}

module Faker.Provider.Name where

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

parseName :: FromJSON a => FakerSettings -> Value -> Parser a
parseName settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  name <- faker .: "name"
  pure name
parseName settings val = fail $ "expected Object, but got " <> (show val)

parseNameField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseNameField settings txt val = do
  name <- parseName settings val
  field <- name .:? txt .!= mempty
  pure field

parseUnresolvedNameField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedNameField settings txt val = do
  name <- parseName settings val
  field <- name .:? txt .!= mempty
  pure $ pure field

parseMaleFirstName ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseMaleFirstName settings = parseNameField settings "male_first_name"

parseFemaleFirstName ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseFemaleFirstName settings = parseNameField settings "female_first_name"

parseFirstName ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser (Unresolved a)
parseFirstName settings = parseUnresolvedNameField settings "first_name"

parseLastName :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseLastName settings = parseNameField settings "last_name"

parsePrefix :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parsePrefix settings = parseNameField settings "prefix"

parseSuffix :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseSuffix settings = parseNameField settings "suffix"

parseFieldName ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser (Unresolved a)
parseFieldName settings = parseUnresolvedNameField settings "name"

parseNameWithMiddle ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser (Unresolved a)
parseNameWithMiddle settings =
  parseUnresolvedNameField settings "name_with_middle"

maleFirstNameProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
maleFirstNameProvider settings = fetchData settings Name parseMaleFirstName

femaleFirstNameProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
femaleFirstNameProvider settings = fetchData settings Name parseFemaleFirstName

firstNameProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
firstNameProvider settings = fetchData settings Name parseFirstName

lastNameProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
lastNameProvider settings = fetchData settings Name parseLastName

prefixProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
prefixProvider settings = fetchData settings Name parsePrefix

suffixProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
suffixProvider settings = fetchData settings Name parseSuffix

nameProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
nameProvider settings = fetchData settings Name parseFieldName

nameWithMiddleProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
nameWithMiddleProvider settings = fetchData settings Name parseNameWithMiddle

resolveNameText :: (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveNameText settings txt = do
  let fields = resolveFields txt
  nameFields <- mapM (resolveNameField settings) fields
  pure $ operateFields txt nameFields

resolveNameField :: (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveNameField settings "female_first_name" =
  randomVec settings femaleFirstNameProvider
resolveNameField settings "male_first_name" =
  randomVec settings maleFirstNameProvider
resolveNameField settings "prefix" = randomVec settings prefixProvider
resolveNameField settings "suffix" = randomVec settings suffixProvider
resolveNameField settings "first_name" =
  randomUnresolvedVec settings firstNameProvider resolveNameText
resolveNameField settings "last_name" = randomVec settings lastNameProvider
resolveNameField settings str = throwM $ InvalidField "name" str
