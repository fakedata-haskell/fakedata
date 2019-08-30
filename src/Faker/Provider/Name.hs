{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Name where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

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

parseNameFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseNameFields settings txts val = do
  name <- parseName settings val
  helper name txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

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

$(genParser "name" "male_first_name")

$(genProvider "name" "male_first_name")

$(genParser "name" "female_first_name")

$(genProvider "name" "female_first_name")

$(genParser "name" "prefix")

$(genProvider "name" "prefix")

$(genParser "name" "suffix")

$(genProvider "name" "suffix")

$(genParser "name" "last_name")

$(genProvider "name" "last_name")

$(genParserUnresolved "name" "name")

$(genProviderUnresolved "name" "name")

$(genParserUnresolved "name" "name_with_middle")

$(genProviderUnresolved "name" "name_with_middle")

$(genParserUnresolved "name" "first_name")

$(genProviderUnresolved "name" "first_name")

resolveNameText :: (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveNameText settings txt = do
  let fields = resolveFields txt
  nameFields <- mapM (resolveNameField settings) fields
  pure $ operateFields txt nameFields

resolveNameField :: (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveNameField settings "female_first_name" =
  randomVec settings nameFemaleFirstNameProvider
resolveNameField settings "male_first_name" =
  randomVec settings nameMaleFirstNameProvider
resolveNameField settings "prefix" = randomVec settings namePrefixProvider
resolveNameField settings "suffix" = randomVec settings nameSuffixProvider
resolveNameField settings "first_name" =
  randomUnresolvedVec settings nameFirstNameProvider resolveNameText
resolveNameField settings "last_name" = randomVec settings nameLastNameProvider
resolveNameField settings str = throwM $ InvalidField "name" str
