{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.StarTrek where

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

parseStarTrek :: FromJSON a => FakerSettings -> Value -> Parser a
parseStarTrek settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  starTrek <- faker .: "star_trek"
  pure starTrek
parseStarTrek settings val = fail $ "expected Object, but got " <> (show val)

parseStarTrekField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseStarTrekField settings txt val = do
  starTrek <- parseStarTrek settings val
  field <- starTrek .:? txt .!= mempty
  pure field

parseStarTrekFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseStarTrekFields settings txts val = do
  starTrek <- parseStarTrek settings val
  helper starTrek txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "starTrek" "character")

$(genProvider "starTrek" "character")

$(genParser "starTrek" "location")

$(genProvider "starTrek" "location")

$(genParser "starTrek" "specie")

$(genProvider "starTrek" "specie")

$(genParser "starTrek" "villain")

$(genProvider "starTrek" "villain")
