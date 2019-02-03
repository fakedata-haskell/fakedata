{-# LANGUAGE OverloadedStrings #-}

module Faker.Provider.Beer where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal

parseBeer :: FromJSON a => FakerSettings -> Value -> Parser a
parseBeer settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  beer <- faker .: "beer"
  pure beer
parseBeer settings val = fail $ "expected Object, but got " <> (show val)

parseBeerField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseBeerField settings txt val = do
  beer <- parseBeer settings val
  field <- beer .:? txt .!= mempty
  pure field

parseBeerName :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseBeerName settings = parseBeerField settings "name"

parseBeerBrand :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseBeerBrand settings = parseBeerField settings "brand"

parseBeerHop :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseBeerHop settings = parseBeerField settings "hop"

parseBeerYeast :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseBeerYeast settings = parseBeerField settings "yeast"

parseBeerMalt :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseBeerMalt settings = parseBeerField settings "malt"

parseBeerStyle :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseBeerStyle settings = parseBeerField settings "style"

beerNameProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
beerNameProvider settings = fetchData settings Beer parseBeerName

beerBrandProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
beerBrandProvider settings = fetchData settings Beer parseBeerBrand

beerHopProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
beerHopProvider settings = fetchData settings Beer parseBeerHop

beerYeastProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
beerYeastProvider settings = fetchData settings Beer parseBeerYeast

beerMaltProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
beerMaltProvider settings = fetchData settings Beer parseBeerMalt

beerStyleProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
beerStyleProvider settings = fetchData settings Beer parseBeerStyle
