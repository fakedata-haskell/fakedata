{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Job where

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
import qualified Data.Aeson.Key as K

parseJob :: FromJSON a => FakerSettings -> Value -> Parser a
parseJob settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  job <- faker .: "job"
  pure job
parseJob settings val = fail $ "expected Object, but got " <> (show val)

parseJobField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseJobField settings txt val = do
  job <- parseJob settings val
  field <- job .:? txt .!= mempty
  pure field

parseJobFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseJobFields settings txts val = do
  job <- parseJob settings val
  helper job txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

parseUnresolvedJobField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> K.Key
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedJobField settings txt val = do
  job <- parseJob settings val
  field <- job .:? txt .!= mempty
  pure $ pure field

parseJobField2 :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseJobField2 settings = parseJobField settings "field"

jobField2Provider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
jobField2Provider settings = fetchData settings Job parseJobField2

$(genParser "job" "seniority")

$(genProvider "job" "seniority")

$(genParser "job" "field_of")

$(genProvider "job" "field_of")

$(genParser "job" "position")

$(genProvider "job" "position")

$(genParser "job" "key_skills")

$(genProvider "job" "key_skills")

$(genParser "job" "employment_type")

$(genProvider "job" "employment_type")

$(genParser "job" "education_level")

$(genProvider "job" "education_level")

$(genParserUnresolved "job" "title")

$(genProviderUnresolved "job" "title")

resolveJobText :: (MonadIO m, MonadThrow m) => FakerSettings -> K.Key -> m Text
resolveJobText = genericResolver' resolveJobField

resolveJobField :: (MonadThrow m, MonadIO m) => FakerSettings -> K.Key -> m Text
resolveJobField settings field@"seniority" =
  cachedRandomVec "job" field jobSeniorityProvider settings
resolveJobField settings field@"field" =
  cachedRandomVec "job" field jobField2Provider settings
resolveJobField settings field@"position" =
  cachedRandomVec "job" field jobPositionProvider settings
resolveJobField settings field@"field_of" =
  cachedRandomVec "job" field jobFieldOfProvider settings
resolveJobField settings field@"title" =
  cachedRandomUnresolvedVec "job" field jobTitleProvider resolveJobText settings
resolveJobField settings str = throwM $ InvalidField "job" str
