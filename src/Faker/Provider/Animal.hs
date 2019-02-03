{-# LANGUAGE OverloadedStrings #-}

module Faker.Provider.Animal where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal

parseAnimal :: FromJSON a => FakerSettings -> Value -> Parser a
parseAnimal settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  animal <- faker .: "animal"
  pure animal
parseAnimal settings val = fail $ "expected Object, but got " <> (show val)

parseAnimalField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseAnimalField settings txt val = do
  animal <- parseAnimal settings val
  field <- animal .:? txt .!= mempty
  pure field

parseAnimalName :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseAnimalName settings = parseAnimalField settings "name"

animalProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
animalProvider settings = fetchData settings Animal parseAnimalName
