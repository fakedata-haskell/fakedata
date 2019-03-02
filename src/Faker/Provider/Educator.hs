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

$(genParser "educator" "name")

$(genProvider "educator" "name")

$(genParser "educator" "secondary")

$(genProvider "educator" "secondary")

$(genParsers "educator" ["tertiary", "type"])

$(genProviders "educator" ["tertiary", "type"])

$(genParsers "educator" ["tertiary", "degree", "subject"])

$(genProviders "educator" ["tertiary", "degree", "subject"])

$(genParsers "educator" ["tertiary", "degree", "type"])

$(genProviders "educator" ["tertiary", "degree", "type"])

resolveEducatorText ::
     (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveEducatorText settings txt = do
  let fields = resolveFields txt
  educatorFields <- mapM (resolveEducatorField settings) fields
  pure $ operateFields txt educatorFields

resolveEducatorField ::
     (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveEducatorField settings "course_number" =
  randomUnresolvedVec
    settings
    educatorTertiaryCourseNumberProvider
    resolveEducatorText
resolveEducatorField settings str = throwM $ InvalidField "educator" str
