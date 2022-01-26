{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Stargate where

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


parseStargate :: FromJSON a => FakerSettings -> Value -> Parser a
parseStargate settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  stargate <- faker .: "stargate"
  pure stargate
parseStargate settings val = fail $ "expected Object, but got " <> (show val)

parseStargateField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseStargateField settings txt val = do
  stargate <- parseStargate settings val
  field <- stargate .:? txt .!= mempty
  pure field

parseStargateFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseStargateFields settings txts val = do
  stargate <- parseStargate settings val
  helper stargate txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "stargate" "characters")

$(genProvider "stargate" "characters")

$(genParser "stargate" "planets")

$(genProvider "stargate" "planets")

$(genParser "stargate" "quotes")

$(genProvider "stargate" "quotes")
