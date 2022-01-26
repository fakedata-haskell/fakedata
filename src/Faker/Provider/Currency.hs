{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Currency where

import Config
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


parseCurrency :: FromJSON a => FakerSettings -> Value -> Parser a
parseCurrency settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  currency <- faker .: "currency"
  pure currency
parseCurrency settings val = fail $ "expected Object, but got " <> (show val)

parseCurrencyField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseCurrencyField settings txt val = do
  currency <- parseCurrency settings val
  field <- currency .:? txt .!= mempty
  pure field

$(genParser "currency" "name")

$(genProvider "currency" "name")

$(genParser "currency" "code")

$(genProvider "currency" "code")

$(genParser "currency" "symbol")

$(genProvider "currency" "symbol")
