{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Buffy where

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

parseBuffy :: FromJSON a => FakerSettings -> Value -> Parser a
parseBuffy settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  buffy <- faker .: "buffy"
  pure buffy
parseBuffy settings val = fail $ "expected Object, but got " <> (show val)

parseBuffyField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseBuffyField settings txt val = do
  buffy <- parseBuffy settings val
  field <- buffy .:? txt .!= mempty
  pure field

parseUnresolvedBuffyField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedBuffyField settings txt val = do
  buffy <- parseBuffy settings val
  field <- buffy .:? txt .!= mempty
  pure $ pure field

$(genParser "buffy" "characters")

$(genProvider "buffy" "characters")

$(genParser "buffy" "quotes")

$(genProvider "buffy" "quotes")

$(genParser "buffy" "actors")

$(genProvider "buffy" "actors")

$(genParser "buffy" "big_bads")

$(genProvider "buffy" "big_bads")

$(genParser "buffy" "episodes")

$(genProvider "buffy" "episodes")
