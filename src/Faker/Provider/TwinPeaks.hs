{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.TwinPeaks where

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


parseTwinPeaks :: FromJSON a => FakerSettings -> Value -> Parser a
parseTwinPeaks settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  twinPeaks <- faker .: "twin_peaks"
  pure twinPeaks
parseTwinPeaks settings val = fail $ "expected Object, but got " <> (show val)

parseTwinPeaksField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseTwinPeaksField settings txt val = do
  twinPeaks <- parseTwinPeaks settings val
  field <- twinPeaks .:? txt .!= mempty
  pure field

parseTwinPeaksFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseTwinPeaksFields settings txts val = do
  twinPeaks <- parseTwinPeaks settings val
  helper twinPeaks txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "twinPeaks" "characters")

$(genProvider "twinPeaks" "characters")

$(genParser "twinPeaks" "locations")

$(genProvider "twinPeaks" "locations")

$(genParser "twinPeaks" "quotes")

$(genProvider "twinPeaks" "quotes")
