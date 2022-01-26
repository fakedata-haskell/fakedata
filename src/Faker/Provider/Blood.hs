{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Blood where

import Config
import Control.Monad.Catch
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH
import Control.Monad.IO.Class


parseBlood :: FromJSON a => FakerSettings -> Value -> Parser a
parseBlood settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  blood <- faker .: "blood"
  pure blood
parseBlood settings val = fail $ "expected Object, but got " <> (show val)

parseBloodField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseBloodField settings txt val = do
  blood <- parseBlood settings val
  field <- blood .:? txt .!= mempty
  pure field

parseBloodFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseBloodFields settings txts val = do
  blood <- parseBlood settings val
  helper blood txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

parseUnresolvedBloodField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> AesonKey
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedBloodField settings txt val = do
  blood <- parseBlood settings val
  field <- blood .:? txt .!= mempty
  pure $ pure field

$(genParser "blood" "type")

$(genProvider "blood" "type")

$(genParser "blood" "rh_factor")

$(genProvider "blood" "rh_factor")

$(genParserUnresolved "blood" "group")

$(genProviderUnresolved "blood" "group")

resolveBloodText :: (MonadIO m, MonadThrow m) => FakerSettings -> AesonKey -> m Text
resolveBloodText = genericResolver' resolveBloodField

resolveBloodField ::
     (MonadThrow m, MonadIO m) => FakerSettings -> AesonKey -> m Text
resolveBloodField settings field@"type" =
  cachedRandomVec "blood" field bloodTypeProvider settings
resolveBloodField settings field@"rh_factor" =
  cachedRandomVec "blood" field bloodRhFactorProvider settings
resolveBloodField settings str = throwM $ InvalidField "blood" str
