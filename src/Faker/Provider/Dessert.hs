{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Dessert where

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

parseDessert :: FromJSON a => FakerSettings -> Value -> Parser a
parseDessert settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  dessert <- faker .: "dessert"
  pure dessert
parseDessert settings val = fail $ "expected Object, but got " <> (show val)

parseDessertField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseDessertField settings txt val = do
  dessert <- parseDessert settings val
  field <- dessert .:? txt .!= mempty
  pure field

$(genParser "dessert" "variety")

$(genProvider "dessert" "variety")

$(genParser "dessert" "topping")

$(genProvider "dessert" "topping")

$(genParser "dessert" "flavor")

$(genProvider "dessert" "flavor")
