{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.DrWho where

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

parseDrWho :: FromJSON a => FakerSettings -> Value -> Parser a
parseDrWho settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  drWho <- faker .: "dr_who"
  pure drWho
parseDrWho settings val = fail $ "expected Object, but got " <> (show val)

parseDrWhoField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseDrWhoField settings txt val = do
  drWho <- parseDrWho settings val
  field <- drWho .:? txt .!= mempty
  pure field

$(genParser "drWho" "character")

$(genProvider "drWho" "character")

$(genParser "drWho" "the_doctors")

$(genProvider "drWho" "the_doctors")

$(genParser "drWho" "actors")

$(genProvider "drWho" "actors")

$(genParser "drWho" "catch_phrases")

$(genProvider "drWho" "catch_phrases")

$(genParser "drWho" "quotes")

$(genProvider "drWho" "quotes")

$(genParser "drWho" "villians")

$(genProvider "drWho" "villians")

$(genParser "drWho" "species")

$(genProvider "drWho" "species")
