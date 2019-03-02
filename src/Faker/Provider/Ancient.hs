{-# LANGUAGE OverloadedStrings #-}

module Faker.Provider.Ancient where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal

parseAncient :: FromJSON a => FakerSettings -> Value -> Parser a
parseAncient settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  ancient <- faker .: "ancient"
  pure ancient
parseAncient settings val = fail $ "expected Object, but got " <> (show val)

parseAncientField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseAncientField settings txt val = do
  ancient <- parseAncient settings val
  field <- ancient .:? txt .!= mempty
  pure field

parseAncientGod :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseAncientGod settings = parseAncientField settings "god"

parseAncientPrimordial ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseAncientPrimordial settings = parseAncientField settings "primordial"

parseAncientTitan ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseAncientTitan settings = parseAncientField settings "titan"

parseAncientHero :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseAncientHero settings = parseAncientField settings "hero"

ancientGodProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
ancientGodProvider settings = fetchData settings Ancient parseAncientGod

ancientPrimordialProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
ancientPrimordialProvider settings =
  fetchData settings Ancient parseAncientPrimordial

ancientTitanProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
ancientTitanProvider settings = fetchData settings Ancient parseAncientTitan

ancientHeroProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
ancientHeroProvider settings = fetchData settings Ancient parseAncientHero
