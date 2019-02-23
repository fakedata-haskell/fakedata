{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.HalfLife where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseHalfLife :: FromJSON a => FakerSettings -> Value -> Parser a
parseHalfLife settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  games <- faker .: "games"
  halfLife <- games .: "half_life"
  pure halfLife
parseHalfLife settings val = fail $ "expected Object, but got " <> (show val)

parseHalfLifeField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseHalfLifeField settings txt val = do
  halfLife <- parseHalfLife settings val
  field <- halfLife .:? txt .!= mempty
  pure field

parseHalfLifeFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseHalfLifeFields settings txts val = do
  halfLife <- parseHalfLife settings val
  helper halfLife txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "halfLife" "character")

$(genProvider "halfLife" "character")

$(genParser "halfLife" "enemy")

$(genProvider "halfLife" "enemy")

$(genParser "halfLife" "location")

$(genProvider "halfLife" "location")
