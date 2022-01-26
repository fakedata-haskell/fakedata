{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Yoda where

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


parseYoda :: FromJSON a => FakerSettings -> Value -> Parser a
parseYoda settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  yoda <- faker .: "yoda"
  pure yoda
parseYoda settings val = fail $ "expected Object, but got " <> (show val)

parseYodaField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseYodaField settings txt val = do
  yoda <- parseYoda settings val
  field <- yoda .:? txt .!= mempty
  pure field

parseYodaFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseYodaFields settings txts val = do
  yoda <- parseYoda settings val
  helper yoda txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "yoda" "quotes")

$(genProvider "yoda" "quotes")
