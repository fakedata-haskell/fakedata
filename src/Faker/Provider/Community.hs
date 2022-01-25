{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Community where

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
import qualified Data.Aeson.Key as K

parseCommunity :: FromJSON a => FakerSettings -> Value -> Parser a
parseCommunity settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  community <- faker .: "community"
  pure community
parseCommunity settings val = fail $ "expected Object, but got " <> (show val)

parseCommunityField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseCommunityField settings txt val = do
  community <- parseCommunity settings val
  field <- community .:? txt .!= mempty
  pure field

$(genParser "community" "characters")

$(genProvider "community" "characters")

$(genParser "community" "quotes")

$(genProvider "community" "quotes")
