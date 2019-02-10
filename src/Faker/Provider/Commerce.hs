{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Commerce where

import Config
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseCommerce :: FromJSON a => FakerSettings -> Value -> Parser a
parseCommerce settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  commerce <- faker .: "commerce"
  pure commerce
parseCommerce settings val = fail $ "expected Object, but got " <> (show val)

parseCommerceField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseCommerceField settings txt val = do
  commerce <- parseCommerce settings val
  field <- commerce .:? txt .!= mempty
  pure field

$(genParser "commerce" "department")

$(genProvider "commerce" "department")
