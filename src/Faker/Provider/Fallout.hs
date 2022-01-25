{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Fallout where

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

parseFallout :: FromJSON a => FakerSettings -> Value -> Parser a
parseFallout settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  games <- faker .: "games"
  fallout <- games .: "fallout"
  pure fallout
parseFallout settings val = fail $ "expected Object, but got " <> (show val)

parseFalloutField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseFalloutField settings txt val = do
  fallout <- parseFallout settings val
  field <- fallout .:? txt .!= mempty
  pure field

parseFalloutFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseFalloutFields settings txts val = do
  fallout <- parseFallout settings val
  helper fallout txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "fallout" "characters")

$(genProvider "fallout" "characters")

$(genParser "fallout" "factions")

$(genProvider "fallout" "factions")

$(genParser "fallout" "locations")

$(genProvider "fallout" "locations")

$(genParser "fallout" "quotes")

$(genProvider "fallout" "quotes")
