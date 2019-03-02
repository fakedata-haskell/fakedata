{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Subscription where

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

parseSubscription :: FromJSON a => FakerSettings -> Value -> Parser a
parseSubscription settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  subscription <- faker .: "subscription"
  pure subscription
parseSubscription settings val =
  fail $ "expected Object, but got " <> (show val)

parseSubscriptionField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseSubscriptionField settings txt val = do
  subscription <- parseSubscription settings val
  field <- subscription .:? txt .!= mempty
  pure field

parseSubscriptionFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseSubscriptionFields settings txts val = do
  subscription <- parseSubscription settings val
  helper subscription txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "subscription" "plans")

$(genProvider "subscription" "plans")

$(genParser "subscription" "statuses")

$(genProvider "subscription" "statuses")

$(genParser "subscription" "payment_methods")

$(genProvider "subscription" "payment_methods")

$(genParser "subscription" "subscription_terms")

$(genProvider "subscription" "subscription_terms")

$(genParser "subscription" "payment_terms")

$(genProvider "subscription" "payment_terms")
