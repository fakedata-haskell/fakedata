{-# LANGUAGE OverloadedStrings #-}

module Faker.Provider.AquaTeenHungerForce where

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


parseAthf :: FromJSON a => FakerSettings -> Value -> Parser a
parseAthf settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  athf <- faker .: "aqua_teen_hunger_force"
  pure athf
parseAthf settings val = fail $ "expected Object, but got " <> (show val)

parseAthfField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseAthfField settings txt val = do
  athf <- parseAthf settings val
  field <- athf .:? txt .!= mempty
  pure field

parseAthfCharacter ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseAthfCharacter settings = parseAthfField settings "character"

athfCharacterProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
athfCharacterProvider settings = fetchData settings ATHF parseAthfCharacter

parseAthfQuote ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseAthfQuote settings = parseAthfField settings "quote"

quoteProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
quoteProvider settings = fetchData settings ATHF parseAthfQuote
