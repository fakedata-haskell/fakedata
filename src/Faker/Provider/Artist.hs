{-# LANGUAGE OverloadedStrings #-}

module Faker.Provider.Artist where

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

parseArtist :: FromJSON a => FakerSettings -> Value -> Parser a
parseArtist settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  artist <- faker .: "artist"
  pure artist
parseArtist settings val = fail $ "expected Object, but got " <> (show val)

parseArtistField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseArtistField settings txt val = do
  artist <- parseArtist settings val
  field <- artist .:? txt .!= mempty
  pure field

parseArtistNames :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseArtistNames settings = parseArtistField settings "names"

artistNamesProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
artistNamesProvider settings = fetchData settings Artist parseArtistNames
