{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Cosmere where

import Config
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

parseCosmere :: FromJSON a => FakerSettings -> Value -> Parser a
parseCosmere settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  cosmere <- faker .: "cosmere"
  pure cosmere
parseCosmere settings val = fail $ "expected Object, but got " <> (show val)

parseCosmereField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseCosmereField settings txt val = do
  cosmere <- parseCosmere settings val
  field <- cosmere .:? txt .!= mempty
  pure field

$(genParser "cosmere" "aons")

$(genProvider "cosmere" "aons")

$(genParser "cosmere" "shard_worlds")

$(genProvider "cosmere" "shard_worlds")

$(genParser "cosmere" "shards")

$(genProvider "cosmere" "shards")

$(genParser "cosmere" "surges")

$(genProvider "cosmere" "surges")

$(genParser "cosmere" "knights_radiant")

$(genProvider "cosmere" "knights_radiant")

$(genParser "cosmere" "metals")

$(genProvider "cosmere" "metals")

$(genParser "cosmere" "allomancers")

$(genProvider "cosmere" "allomancers")

$(genParser "cosmere" "feruchemists")

$(genProvider "cosmere" "feruchemists")

$(genParser "cosmere" "heralds")

$(genProvider "cosmere" "heralds")

$(genParser "cosmere" "sprens")

$(genProvider "cosmere" "sprens")
