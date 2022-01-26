{-# LANGUAGE OverloadedStrings #-}

module Faker.Provider.Animal where

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


parseAnimal :: FromJSON a => FakerSettings -> Value -> Parser a
parseAnimal settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  creature <- faker .: "creature"
  animal <- creature .: "animal"
  pure animal
parseAnimal settings val = fail $ "expected Object, but got " <> (show val)

parseAnimalField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseAnimalField settings txt val = do
  animal <- parseAnimal settings val
  field <- animal .:? txt .!= mempty
  pure field

parseAnimalName :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseAnimalName settings = parseAnimalField settings "name"

animalNameProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
animalNameProvider settings = fetchData settings Animal parseAnimalName
