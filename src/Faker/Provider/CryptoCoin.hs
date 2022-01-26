{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.CryptoCoin where

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


parseCryptoCoin :: FromJSON a => FakerSettings -> Value -> Parser a
parseCryptoCoin settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  cryptoCoin <- faker .: "crypto_coin"
  pure cryptoCoin
parseCryptoCoin settings val = fail $ "expected Object, but got " <> (show val)

parseCryptoCoinField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseCryptoCoinField settings txt val = do
  cryptoCoin <- parseCryptoCoin settings val
  field <- cryptoCoin .:? txt .!= mempty
  pure field

$(genParser "cryptoCoin" "coin")

$(genProvider "cryptoCoin" "coin")
