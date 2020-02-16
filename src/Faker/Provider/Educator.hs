{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Educator where

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

parseEducator :: FromJSON a => FakerSettings -> Value -> Parser a
parseEducator settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  educator <- faker .: "educator"
  pure educator
parseEducator settings val = fail $ "expected Object, but got " <> (show val)

parseEducatorField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseEducatorField settings txt val = do
  educator <- parseEducator settings val
  field <- educator .:? txt .!= mempty
  pure field

parseEducatorFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseEducatorFields settings txts val = do
  educator <- parseEducator settings val
  helper educator txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

parseUnresolvedEducatorField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedEducatorField settings txt val = do
  educator <- parseEducator settings val
  field <- educator .:? txt .!= mempty
  pure $ pure field

parseUnresolvedEducatorFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [Text]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedEducatorFields settings txts val = do
  educator <- parseEducator settings val
  helper educator txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)

parseEducatorTertiaryCourseNumber ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser (Unresolved a)
parseEducatorTertiaryCourseNumber settings =
  parseUnresolvedEducatorFields settings ["tertiary", "degree", "course_number"]

educatorTertiaryCourseNumberProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
educatorTertiaryCourseNumberProvider settings =
  fetchData settings Educator parseEducatorTertiaryCourseNumber

$(genParser "educator" "school_name")

$(genProvider "educator" "school_name")

$(genParser "educator" "secondary")

$(genProvider "educator" "secondary")

$(genParserUnresolved "educator" "university")

$(genProviderUnresolved "educator" "university")

$(genParserUnresolved "educator" "secondary_school")

$(genProviderUnresolved "educator" "secondary_school")

$(genParserUnresolved "educator" "campus")

$(genProviderUnresolved "educator" "campus")

$(genParser "educator" "subject")

$(genProvider "educator" "subject")

$(genParserUnresolved "educator" "degree")

$(genProviderUnresolved "educator" "degree")

$(genParserUnresolved "educator" "course_name")

$(genProviderUnresolved "educator" "course_name")

$(genParsers "educator" ["tertiary", "university_type"])

$(genProviders "educator" ["tertiary", "university_type"])

$(genParsers "educator" ["tertiary", "degree", "type"])

$(genProviders "educator" ["tertiary", "degree", "type"])

$(genParserUnresolveds "educator" ["tertiary", "degree", "course_number"])

$(genProviderUnresolveds "educator" ["tertiary", "degree", "course_number"])

resolveEducatorText ::
     (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveEducatorText settings txt = do
  let fields = resolveFields txt
  educatorFields <-
    mapM
      (\(seed, field) ->
         resolveEducatorField (modifyRandomGen settings seed) field)
      (zip [1 ..] fields)
  pure $ operateFields txt educatorFields

resolveEducatorField ::
     (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveEducatorField settings field@"course_number" =
  cachedRandomUnresolvedVec
    "educator"
    field
    educatorTertiaryCourseNumberProvider
    resolveEducatorText
    settings
resolveEducatorField settings field@"Educator.school_name" =
  cachedRandomVec "educator" "school_name" educatorSchoolNameProvider settings
resolveEducatorField settings field@"Educator.tertiary.university_type" =
  cachedRandomVec "educator" "university_type" educatorTertiaryUniversityTypeProvider settings
resolveEducatorField settings field@"secondary" =
  cachedRandomVec "educator" field educatorSecondaryProvider settings
resolveEducatorField settings field@"Educator.tertiary.degree.type" =
  cachedRandomVec "educator" "type" educatorTertiaryDegreeTypeProvider settings
resolveEducatorField settings field@"subject" =
  cachedRandomVec "educator" field educatorSubjectProvider settings
resolveEducatorField settings field@"Educator.tertiary.degree.course_number" =
  cachedRandomUnresolvedVec
    "educator"
    "course_number"
    educatorTertiaryDegreeCourseNumberProvider
    resolveEducatorText
    settings
resolveEducatorField settings str = throwM $ InvalidField "educator" str
