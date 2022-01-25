{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Verbs where

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

parseVerbs :: FromJSON a => FakerSettings -> Value -> Parser a
parseVerbs settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  verbs <- faker .: "verbs"
  pure verbs
parseVerbs settings val = fail $ "expected Object, but got " <> (show val)

parseVerbsField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseVerbsField settings txt val = do
  verbs <- parseVerbs settings val
  field <- verbs .:? txt .!= mempty
  pure field

parseVerbsFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseVerbsFields settings txts val = do
  verbs <- parseVerbs settings val
  helper verbs txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "verbs" "base")

$(genProvider "verbs" "base")

$(genParser "verbs" "past")

$(genProvider "verbs" "past")

$(genParser "verbs" "past_participle")

$(genProvider "verbs" "past_participle")

$(genParser "verbs" "simple_present")

$(genProvider "verbs" "simple_present")

$(genParser "verbs" "ing_form")

$(genProvider "verbs" "ing_form")
