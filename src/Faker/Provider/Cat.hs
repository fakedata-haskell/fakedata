{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Cat where

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

parseCat :: FromJSON a => FakerSettings -> Value -> Parser a
parseCat settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  creature <- faker .: "creature"
  cat <- creature .: "cat"
  pure cat
parseCat settings val = fail $ "expected Object, but got " <> (show val)

parseCatField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseCatField settings txt val = do
  cat <- parseCat settings val
  field <- cat .:? txt .!= mempty
  pure field

$(genParser "cat" "name")

$(genProvider "cat" "name")

$(genParser "cat" "breed")

$(genProvider "cat" "breed")

$(genParser "cat" "registry")

$(genProvider "cat" "registry")
