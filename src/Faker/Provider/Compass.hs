{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Compass where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseCompass :: FromJSON a => FakerSettings -> Value -> Parser a
parseCompass settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  compass <- faker .: "compass"
  pure compass
parseCompass settings val = fail $ "expected Object, but got " <> (show val)

parseCompassField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseCompassField settings txt val = do
  compass <- parseCompass settings val
  field <- compass .:? txt .!= mempty
  pure field

parseCompassFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseCompassFields settings txts val = do
  compass <- parseCompass settings val
  helper compass txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

parseUnresolvedCompassField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
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
     (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveCompassText settings txt = do
  let fields = resolveFields txt
  compassFields <- mapM (resolveCompassField settings) fields
  pure $ operateFields txt compassFields

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
  cw <- compassHalfwindWordProvider settings
  ca <- compassHalfwindAbbreviationProvider settings
  caz <- compassHalfwindAzimuthProvider settings
  pure $ cw <> ca <> caz

quarterWindProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
quarterWindProvider settings = do
  cw <- compassQuarterwindWordProvider settings
  ca <- compassQuarterwindAbbreviationProvider settings
  caz <- compassQuarterwindAzimuthProvider settings
  pure $ cw <> ca <> caz

abbreviationProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
abbreviationProvider settings = do
  cw <- compassCardinalAbbreviationProvider settings
  ca <- compassOrdinalAbbreviationProvider settings
  caz <- compassHalfwindAbbreviationProvider settings
  qw <- compassQuarterwindAbbreviationProvider settings
  pure $ cw <> ca <> caz <> qw

resolveCompassField ::
     (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveCompassField settings "cardinal" = randomVec settings cardinalProvider
resolveCompassField settings "ordinal" = randomVec settings ordinalProvider
resolveCompassField settings "half_wind" = randomVec settings halfWindProvider
resolveCompassField settings "quarter_wind" =
  randomVec settings quarterWindProvider
resolveCompassField settings "cardinal_abbreviation" =
  randomVec settings compassCardinalAbbreviationProvider
resolveCompassField settings "ordinal_abbreviation" =
  randomVec settings compassOrdinalAbbreviationProvider
resolveCompassField settings "half_wind_abbreviation" =
  randomVec settings compassHalfwindAbbreviationProvider
resolveCompassField settings "quarter_wind_abbreviation" =
  randomVec settings compassQuarterwindAbbreviationProvider
resolveCompassField settings "cardinal_azimuth" =
  randomVec settings compassCardinalAzimuthProvider
resolveCompassField settings "ordinal_azimuth" =
  randomVec settings compassOrdinalAzimuthProvider
resolveCompassField settings "half_wind_azimuth" =
  randomVec settings compassHalfwindAzimuthProvider
resolveCompassField settings "quarter_wind_azimuth" =
  randomVec settings compassQuarterwindAzimuthProvider
resolveCompassField settings "direction" =
  randomUnresolvedVec settings compassDirectionProvider resolveCompassText
resolveCompassField settings "abbreviation" =
  randomUnresolvedVec settings compassAbbreviationProvider resolveCompassText
resolveCompassField settings "azimuth" =
  randomUnresolvedVec settings compassAzimuthProvider resolveCompassText
resolveCompassField settings str = throwM $ InvalidField "compass" str
