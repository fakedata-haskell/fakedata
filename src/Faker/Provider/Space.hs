{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Space where

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

parseSpace :: FromJSON a => FakerSettings -> Value -> Parser a
parseSpace settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  space <- faker .: "space"
  pure space
parseSpace settings val = fail $ "expected Object, but got " <> (show val)

parseSpaceField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseSpaceField settings txt val = do
  space <- parseSpace settings val
  field <- space .:? txt .!= mempty
  pure field

parseSpaceFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseSpaceFields settings txts val = do
  space <- parseSpace settings val
  helper space txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "space" "planet")

$(genProvider "space" "planet")

$(genParser "space" "moon")

$(genProvider "space" "moon")

$(genParser "space" "galaxy")

$(genProvider "space" "galaxy")

$(genParser "space" "nebula")

$(genProvider "space" "nebula")

$(genParser "space" "star_cluster")

$(genProvider "space" "star_cluster")

$(genParser "space" "constellation")

$(genProvider "space" "constellation")

$(genParser "space" "star")

$(genProvider "space" "star")

$(genParser "space" "agency")

$(genProvider "space" "agency")

$(genParser "space" "agency_abv")

$(genProvider "space" "agency_abv")

$(genParser "space" "nasa_space_craft")

$(genProvider "space" "nasa_space_craft")

$(genParser "space" "company")

$(genProvider "space" "company")

$(genParser "space" "distance_measurement")

$(genProvider "space" "distance_measurement")

$(genParser "space" "meteorite")

$(genProvider "space" "meteorite")

$(genParser "space" "launch_vehicle")

$(genProvider "space" "launch_vehicle")
