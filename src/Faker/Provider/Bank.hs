{-# LANGUAGE OverloadedStrings #-}

module Faker.Provider.Bank where

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

parseBank :: FromJSON a => FakerSettings -> Value -> Parser a
parseBank settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  bank <- faker .: "bank"
  pure bank
parseBank settings val = fail $ "expected Object, but got " <> (show val)

parseBankField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseBankField settings txt val = do
  bank <- parseBank settings val
  field <- bank .:? txt .!= mempty
  pure field

parseBankName :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseBankName settings = parseBankField settings "name"

parseBankSwiftBic ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseBankSwiftBic settings = parseBankField settings "swift_bic"

bankNameProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
bankNameProvider settings = fetchData settings Bank parseBankName

bankSwiftBicProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
bankSwiftBicProvider settings = fetchData settings Bank parseBankName
