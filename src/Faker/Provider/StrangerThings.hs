{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.StrangerThings where

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


parseStrangerThings :: FromJSON a => FakerSettings -> Value -> Parser a
parseStrangerThings settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  strangerThings <- faker .: "stranger_things"
  pure strangerThings
parseStrangerThings settings val =
  fail $ "expected Object, but got " <> (show val)

parseStrangerThingsField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseStrangerThingsField settings txt val = do
  strangerThings <- parseStrangerThings settings val
  field <- strangerThings .:? txt .!= mempty
  pure field

parseStrangerThingsFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseStrangerThingsFields settings txts val = do
  strangerThings <- parseStrangerThings settings val
  helper strangerThings txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "strangerThings" "character")

$(genProvider "strangerThings" "character")

$(genParser "strangerThings" "quote")

$(genProvider "strangerThings" "quote")
