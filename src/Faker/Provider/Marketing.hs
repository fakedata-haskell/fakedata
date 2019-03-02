{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Marketing where

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

parseMarketing :: FromJSON a => FakerSettings -> Value -> Parser a
parseMarketing settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  marketing <- faker .: "marketing"
  pure marketing
parseMarketing settings val = fail $ "expected Object, but got " <> (show val)

parseMarketingField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseMarketingField settings txt val = do
  marketing <- parseMarketing settings val
  field <- marketing .:? txt .!= mempty
  pure field

parseMarketingFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseMarketingFields settings txts val = do
  marketing <- parseMarketing settings val
  helper marketing txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "marketing" "buzzwords")

$(genProvider "marketing" "buzzwords")
