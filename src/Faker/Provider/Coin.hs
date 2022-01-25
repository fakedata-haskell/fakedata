{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Coin where

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
import qualified Data.Aeson.Key as K

parseCoin :: FromJSON a => FakerSettings -> Value -> Parser a
parseCoin settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  coin <- faker .: "coin"
  pure coin
parseCoin settings val = fail $ "expected Object, but got " <> (show val)

parseCoinField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseCoinField settings txt val = do
  coin <- parseCoin settings val
  field <- coin .:? txt .!= mempty
  pure field

$(genParser "coin" "flip")

$(genProvider "coin" "flip")
