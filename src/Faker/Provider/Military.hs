{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Military where

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

parseMilitary :: FromJSON a => FakerSettings -> Value -> Parser a
parseMilitary settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  military <- faker .: "military"
  pure military
parseMilitary settings val = fail $ "expected Object, but got " <> (show val)

parseMilitaryField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseMilitaryField settings txt val = do
  military <- parseMilitary settings val
  field <- military .:? txt .!= mempty
  pure field

parseMilitaryFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseMilitaryFields settings txts val = do
  military <- parseMilitary settings val
  helper military txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "military" "army_rank")

$(genProvider "military" "army_rank")

$(genParser "military" "marines_rank")

$(genProvider "military" "marines_rank")

$(genParser "military" "navy_rank")

$(genProvider "military" "navy_rank")

$(genParser "military" "air_force_rank")

$(genProvider "military" "air_force_rank")

$(genParser "military" "dod_paygrade")

$(genProvider "military" "dod_paygrade")

$(genParser "military" "coast_guard_rank")

$(genProvider "military" "coast_guard_rank")

$(genParser "military" "space_force_rank")

$(genProvider "military" "space_force_rank")
