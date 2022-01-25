{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Business where

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
import Faker.Provider.TH
import Language.Haskell.TH
import qualified Data.Aeson.Key as K

parseBusiness :: FromJSON a => FakerSettings -> Value -> Parser a
parseBusiness settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  business <- faker .: "business"
  pure business
parseBusiness settings val = fail $ "expected Object, but got " <> (show val)

parseBusinessField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseBusinessField settings txt val = do
  business <- parseBusiness settings val
  field <- business .:? txt .!= mempty
  pure field

$(genParser "business" "credit_card_numbers")

$(genProvider "business" "credit_card_numbers")

$(genParser "business" "credit_card_types")

$(genProvider "business" "credit_card_types")
