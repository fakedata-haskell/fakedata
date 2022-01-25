{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.DumbAndDumber where

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

parseDumbAndDumber :: FromJSON a => FakerSettings -> Value -> Parser a
parseDumbAndDumber settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  dumbAndDumber <- faker .: "dumb_and_dumber"
  pure dumbAndDumber
parseDumbAndDumber settings val =
  fail $ "expected Object, but got " <> (show val)

parseDumbAndDumberField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseDumbAndDumberField settings txt val = do
  dumbAndDumber <- parseDumbAndDumber settings val
  field <- dumbAndDumber .:? txt .!= mempty
  pure field

$(genParser "dumbAndDumber" "actors")

$(genProvider "dumbAndDumber" "actors")

$(genParser "dumbAndDumber" "characters")

$(genProvider "dumbAndDumber" "characters")

$(genParser "dumbAndDumber" "quotes")

$(genProvider "dumbAndDumber" "quotes")
