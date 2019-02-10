{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Dota where

import Config
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseDota :: FromJSON a => FakerSettings -> Value -> Parser a
parseDota settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  dota <- faker .: "dota"
  pure dota
parseDota settings val = fail $ "expected Object, but got " <> (show val)

parseDotaField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseDotaField settings txt val = do
  dota <- parseDota settings val
  field <- dota .:? txt .!= mempty
  pure field

$(genParser "dota" "hero")

$(genProvider "dota" "hero")

$(genParser "dota" "item")

$(genProvider "dota" "item")

$(genParser "dota" "team")

$(genProvider "dota" "team")

$(genParser "dota" "player")

$(genProvider "dota" "player")
