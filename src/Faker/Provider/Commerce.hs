{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Commerce where

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


parseCommerce :: FromJSON a => FakerSettings -> Value -> Parser a
parseCommerce settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  commerce <- faker .: "commerce"
  pure commerce
parseCommerce settings val = fail $ "expected Object, but got " <> (show val)

parseCommerceField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseCommerceField settings txt val = do
  commerce <- parseCommerce settings val
  field <- commerce .:? txt .!= mempty
  pure field

parseCommerceFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseCommerceFields settings txts val = do
  commerce <- parseCommerce settings val
  helper commerce txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "commerce" "department")

$(genProvider "commerce" "department")

$(genParsers "commerce" ["product_name", "adjective"])

$(genProviders "commerce" ["product_name", "adjective"])

$(genParsers "commerce" ["product_name", "material"])

$(genProviders "commerce" ["product_name", "material"])

$(genParsers "commerce" ["product_name", "product"])

$(genProviders "commerce" ["product_name", "product"])

$(genParsers "commerce" ["promotion_code", "adjective"])

$(genProviders "commerce" ["promotion_code", "adjective"])

$(genParsers "commerce" ["promotion_code", "noun"])

$(genProviders "commerce" ["promotion_code", "noun"])
