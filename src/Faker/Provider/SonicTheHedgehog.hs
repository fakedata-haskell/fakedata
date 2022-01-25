{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.SonicTheHedgehog where

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

parseSonicTheHedgehog :: FromJSON a => FakerSettings -> Value -> Parser a
parseSonicTheHedgehog settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  games <- faker .: "games"
  sonicTheHedgehog <- games .: "sonic_the_hedgehog"
  pure sonicTheHedgehog
parseSonicTheHedgehog settings val =
  fail $ "expected Object, but got " <> (show val)

parseSonicTheHedgehogField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseSonicTheHedgehogField settings txt val = do
  sonicTheHedgehog <- parseSonicTheHedgehog settings val
  field <- sonicTheHedgehog .:? txt .!= mempty
  pure field

parseSonicTheHedgehogFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseSonicTheHedgehogFields settings txts val = do
  sonicTheHedgehog <- parseSonicTheHedgehog settings val
  helper sonicTheHedgehog txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "sonicTheHedgehog" "zone")

$(genProvider "sonicTheHedgehog" "zone")

$(genParser "sonicTheHedgehog" "character")

$(genProvider "sonicTheHedgehog" "character")

$(genParser "sonicTheHedgehog" "game")

$(genProvider "sonicTheHedgehog" "game")
