{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Hacker where

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

parseHacker :: FromJSON a => FakerSettings -> Value -> Parser a
parseHacker settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  hacker <- faker .: "hacker"
  pure hacker
parseHacker settings val = fail $ "expected Object, but got " <> (show val)

parseHackerField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseHackerField settings txt val = do
  hacker <- parseHacker settings val
  field <- hacker .:? txt .!= mempty
  pure field

parseHackerFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseHackerFields settings txts val = do
  hacker <- parseHacker settings val
  helper hacker txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "hacker" "abbreviation")

$(genProvider "hacker" "abbreviation")

$(genParser "hacker" "adjective")

$(genProvider "hacker" "adjective")

$(genParser "hacker" "noun")

$(genProvider "hacker" "noun")

$(genParser "hacker" "verb")

$(genProvider "hacker" "verb")

$(genParser "hacker" "ingverb")

$(genProvider "hacker" "ingverb")
