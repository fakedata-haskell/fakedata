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
  nameFields <-
    mapM
      (\(seed, field) -> resolveNameField (modifyRandomGen settings seed) field)
      (zip [1 ..] fields)
  pure $ operateFields txt nameFields

resolveNameField :: (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveNameField settings field@"female_first_name" =
  cachedRandomVec "name" field nameFemaleFirstNameProvider settings
resolveNameField settings field@"male_first_name" =
  cachedRandomVec "name" field nameMaleFirstNameProvider settings
resolveNameField settings field@"prefix" =
  cachedRandomVec "name" field namePrefixProvider settings
resolveNameField settings field@"suffix" =
  cachedRandomVec "name" field nameSuffixProvider settings
resolveNameField settings field@"first_name" =
  cachedRandomUnresolvedVec
    "name"
    field
    nameFirstNameProvider
    resolveNameText
    settings
resolveNameField settings field@"last_name" =
  cachedRandomVec "name" field nameLastNameProvider settings
resolveNameField settings field@"male_last_name" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parseNameField settings field
    provider settings = fetchData settings Name parser
    in cachedRandomVec "name" field provider settings
resolveNameField settings field@"female_last_name" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parseNameField settings field
    provider settings = fetchData settings Name parser
    in cachedRandomVec "name" field provider settings
resolveNameField settings field@"female_middle_name" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parseNameField settings field
    provider settings = fetchData settings Name parser
    in cachedRandomVec "name" field provider settings
resolveNameField settings field@"male_middle_name" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parseNameField settings field
    provider settings = fetchData settings Name parser
    in cachedRandomVec "name" field provider settings
resolveNameField settings field@"middle_name" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parseNameField settings field
    provider settings = fetchData settings Name parser
    in cachedRandomVec "name" field provider settings
resolveNameField settings field@"male_prefix" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parseNameField settings field
    provider settings = fetchData settings Name parser
    in cachedRandomVec "name" field provider settings
resolveNameField settings field@"female_prefix" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parseNameField settings field
    provider settings = fetchData settings Name parser
    in cachedRandomVec "name" field provider settings
resolveNameField settings field@"nobility_title_prefix" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parseNameField settings field
    provider settings = fetchData settings Name parser
    in cachedRandomVec "name" field provider settings
resolveNameField settings field@"nobility_title" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parseNameField settings field
    provider settings = fetchData settings Name parser
    in cachedRandomVec "name" field provider settings
resolveNameField settings field@"malay_male_first_name" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parseNameField settings field
    provider settings = fetchData settings Name parser
    in cachedRandomVec "name" field provider settings
resolveNameField settings field@"malay_female_first_name" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parseNameField settings field
    provider settings = fetchData settings Name parser
    in cachedRandomVec "name" field provider settings
resolveNameField settings field@"chinese_male_first_name" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parseNameField settings field
    provider settings = fetchData settings Name parser
    in cachedRandomVec "name" field provider settings
resolveNameField settings field@"chinese_male_last_name" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parseNameField settings field
    provider settings = fetchData settings Name parser
    in cachedRandomVec "name" field provider settings
resolveNameField settings field@"chinese_female_first_name" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parseNameField settings field
    provider settings = fetchData settings Name parser
    in cachedRandomVec "name" field provider settings
resolveNameField settings field@"male_english_name" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parseNameField settings field
    provider settings = fetchData settings Name parser
    in cachedRandomVec "name" field provider settings
resolveNameField settings field@"female_english_name" = let
    parser ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
    parser settings = parseNameField settings field
    provider settings = fetchData settings Name parser
    in cachedRandomVec "name" field provider settings
resolveNameField settings str = throwM $ InvalidField "name" str
