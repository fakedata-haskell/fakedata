{-# LANGUAGE OverloadedStrings #-}

module Faker.Provider.BoJackHorseman where

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


parseBoJackHorseman :: FromJSON a => FakerSettings -> Value -> Parser a
parseBoJackHorseman settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  boJackHorseman <- faker .: "bojack_horseman"
  pure boJackHorseman
parseBoJackHorseman settings val =
  fail $ "expected Object, but got " <> (show val)

parseBoJackHorsemanField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseBoJackHorsemanField settings txt val = do
  boJackHorseman <- parseBoJackHorseman settings val
  field <- boJackHorseman .:? txt .!= mempty
  pure field

parseBoJackHorsemanCharacter ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseBoJackHorsemanCharacter settings =
  parseBoJackHorsemanField settings "characters"

parseBoJackHorsemanQuote ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseBoJackHorsemanQuote settings = parseBoJackHorsemanField settings "quotes"

parseBoJackHorsemanTongueTwister ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseBoJackHorsemanTongueTwister settings =
  parseBoJackHorsemanField settings "tongue_twisters"

boJackHorsemanCharacterProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
boJackHorsemanCharacterProvider settings =
  fetchData settings BoJackHorseman parseBoJackHorsemanCharacter

boJackHorsemanQuoteProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
boJackHorsemanQuoteProvider settings =
  fetchData settings BoJackHorseman parseBoJackHorsemanQuote

boJackHorsemanTongueTwisterProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
boJackHorsemanTongueTwisterProvider settings =
  fetchData settings BoJackHorseman parseBoJackHorsemanTongueTwister
