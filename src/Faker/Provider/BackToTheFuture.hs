{-# LANGUAGE OverloadedStrings #-}

module Faker.Provider.BackToTheFuture where

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

parseBttf :: FromJSON a => FakerSettings -> Value -> Parser a
parseBttf settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  bttf <- faker .: "back_to_the_future"
  pure bttf
parseBttf settings val = fail $ "expected Object, but got " <> (show val)

parseBttfField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseBttfField settings txt val = do
  bttf <- parseBttf settings val
  field <- bttf .:? txt .!= mempty
  pure field

parseBttfCharacter ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseBttfCharacter settings = parseBttfField settings "characters"

bttfCharacterProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
bttfCharacterProvider settings = fetchData settings BTTF parseBttfCharacter

parseBttfDates :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseBttfDates settings = parseBttfField settings "dates"

bttfDatesProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
bttfDatesProvider settings = fetchData settings BTTF parseBttfDates

parseBttfQuote :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseBttfQuote settings = parseBttfField settings "quotes"

bttfQuoteProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
bttfQuoteProvider settings = fetchData settings BTTF parseBttfQuote
