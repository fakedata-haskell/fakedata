{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Control where

import Config
import Control.Monad.Catch
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH


parseControl :: FromJSON a => FakerSettings -> Value -> Parser a
parseControl settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  games <- faker .: "games"
  control <- games .: "control"
  pure control
parseControl settings val = fail $ "expected Object, but got " <> (show val)

parseControlField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseControlField settings txt val = do
  control <- parseControl settings val
  field <- control .:? txt .!= mempty
  pure field

parseControlFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseControlFields settings txts val = do
  control <- parseControl settings val
  helper control txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "control" "character")

$(genProvider "control" "character")

$(genParser "control" "location")

$(genProvider "control" "location")

$(genParser "control" "object_of_power")

$(genProvider "control" "object_of_power")

$(genParser "control" "altered_item")

$(genProvider "control" "altered_item")

$(genParser "control" "altered_world_event")

$(genProvider "control" "altered_world_event")

$(genParser "control" "hiss")

$(genProvider "control" "hiss")

$(genParser "control" "the_board")

$(genProvider "control" "the_board")

$(genParser "control" "quote")

$(genProvider "control" "quote")
