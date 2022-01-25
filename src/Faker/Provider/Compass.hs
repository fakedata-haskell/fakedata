{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Compass where

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

parseCompass :: FromJSON a => FakerSettings -> Value -> Parser a
parseCompass settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  compass <- faker .: "compass"
  pure compass
parseCompass settings val = fail $ "expected Object, but got " <> (show val)

parseCompassField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseCompassField settings txt val = do
  compass <- parseCompass settings val
  field <- compass .:? txt .!= mempty
  pure field

parseCompassFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseCompassFields settings txts val = do
  compass <- parseCompass settings val
  helper compass txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

parseUnresolvedCompassField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> K.Key
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedCompassField settings txt val = do
  compass <- parseCompass settings val
  field <- compass .:? txt .!= mempty
  pure $ pure field

$(genParserUnresolved "compass" "direction")

$(genProviderUnresolved "compass" "direction")

$(genParserUnresolved "compass" "abbreviation")

$(genProviderUnresolved "compass" "abbreviation")

$(genParserUnresolved "compass" "azimuth")

$(genProviderUnresolved "compass" "azimuth")

$(genParsers "compass" ["cardinal", "word"])

$(genProviders "compass" ["cardinal", "word"])

$(genParsers "compass" ["cardinal", "abbreviation"])

$(genProviders "compass" ["cardinal", "abbreviation"])

$(genParsers "compass" ["cardinal", "azimuth"])

$(genProviders "compass" ["cardinal", "azimuth"])

$(genParsers "compass" ["ordinal", "word"])

$(genProviders "compass" ["ordinal", "word"])

$(genParsers "compass" ["ordinal", "abbreviation"])

$(genProviders "compass" ["ordinal", "abbreviation"])

$(genParsers "compass" ["ordinal", "azimuth"])

$(genProviders "compass" ["ordinal", "azimuth"])

$(genParsers "compass" ["half-wind", "word"])

$(genProviders "compass" ["half-wind", "word"])

$(genParsers "compass" ["half-wind", "abbreviation"])

$(genProviders "compass" ["half-wind", "abbreviation"])

$(genParsers "compass" ["half-wind", "azimuth"])

$(genProviders "compass" ["half-wind", "azimuth"])

$(genParsers "compass" ["quarter-wind", "word"])

$(genProviders "compass" ["quarter-wind", "word"])

$(genParsers "compass" ["quarter-wind", "abbreviation"])

$(genProviders "compass" ["quarter-wind", "abbreviation"])

$(genParsers "compass" ["quarter-wind", "azimuth"])

$(genProviders "compass" ["quarter-wind", "azimuth"])

resolveCompassText ::
     (MonadIO m, MonadThrow m) => FakerSettings -> K.Key -> m Text
resolveCompassText = genericResolver' resolveCompassField

cardinalProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
cardinalProvider settings = do
  cw <- compassCardinalWordProvider settings
  ca <- compassCardinalAbbreviationProvider settings
  caz <- compassCardinalAzimuthProvider settings
  pure $ cw <> ca <> caz

ordinalProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
ordinalProvider settings = do
  cw <- compassOrdinalWordProvider settings
  ca <- compassOrdinalAbbreviationProvider settings
  caz <- compassOrdinalAzimuthProvider settings
  pure $ cw <> ca <> caz

halfWindProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
halfWindProvider settings = do
  cw <- compassHalfWindWordProvider settings
  ca <- compassHalfWindAbbreviationProvider settings
  caz <- compassHalfWindAzimuthProvider settings
  pure $ cw <> ca <> caz

quarterWindProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
quarterWindProvider settings = do
  cw <- compassQuarterWindWordProvider settings
  ca <- compassQuarterWindAbbreviationProvider settings
  caz <- compassQuarterWindAzimuthProvider settings
  pure $ cw <> ca <> caz

abbreviationProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
abbreviationProvider settings = do
  cw <- compassCardinalAbbreviationProvider settings
  ca <- compassOrdinalAbbreviationProvider settings
  caz <- compassHalfWindAbbreviationProvider settings
  qw <- compassQuarterWindAbbreviationProvider settings
  pure $ cw <> ca <> caz <> qw

resolveCompassField ::
     (MonadThrow m, MonadIO m) => FakerSettings -> K.Key -> m Text
resolveCompassField settings field@"cardinal" =
  cachedRandomVec "compass" field cardinalProvider settings
resolveCompassField settings field@"ordinal" =
  cachedRandomVec "compass" field ordinalProvider settings
resolveCompassField settings field@"half_wind" =
  cachedRandomVec "compass" field halfWindProvider settings
resolveCompassField settings field@"quarter_wind" =
  cachedRandomVec "compass" field quarterWindProvider settings
resolveCompassField settings field@"cardinal_abbreviation" =
  cachedRandomVec "compass" field compassCardinalAbbreviationProvider settings
resolveCompassField settings field@"ordinal_abbreviation" =
  cachedRandomVec "compass" field compassOrdinalAbbreviationProvider settings
resolveCompassField settings field@"half_wind_abbreviation" =
  cachedRandomVec "compass" field compassHalfWindAbbreviationProvider settings
resolveCompassField settings field@"quarter_wind_abbreviation" =
  cachedRandomVec
    "compass"
    field
    compassQuarterWindAbbreviationProvider
    settings
resolveCompassField settings field@"cardinal_azimuth" =
  cachedRandomVec "compass" field compassCardinalAzimuthProvider settings
resolveCompassField settings field@"ordinal_azimuth" =
  cachedRandomVec "compass" field compassOrdinalAzimuthProvider settings
resolveCompassField settings field@"half_wind_azimuth" =
  cachedRandomVec "compass" field compassHalfWindAzimuthProvider settings
resolveCompassField settings field@"quarter_wind_azimuth" =
  cachedRandomVec "compass" field compassQuarterWindAzimuthProvider settings
resolveCompassField settings field@"direction" =
  cachedRandomUnresolvedVec
    "compass"
    field
    compassDirectionProvider
    resolveCompassText
    settings
resolveCompassField settings field@"abbreviation" =
  cachedRandomUnresolvedVec
    "compass"
    field
    compassAbbreviationProvider
    resolveCompassText
    settings
resolveCompassField settings field@"azimuth" =
  cachedRandomUnresolvedVec
    "compass"
    field
    compassAzimuthProvider
    resolveCompassText
    settings
resolveCompassField settings str = throwM $ InvalidField "compass" str
