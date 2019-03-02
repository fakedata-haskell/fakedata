{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.WorldOfWarcraft where

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

parseWorldOfWarcraft :: FromJSON a => FakerSettings -> Value -> Parser a
parseWorldOfWarcraft settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  games <- faker .: "games"
  worldOfWarcraft <- games .: "world_of_warcraft"
  pure worldOfWarcraft
parseWorldOfWarcraft settings val =
  fail $ "expected Object, but got " <> (show val)

parseWorldOfWarcraftField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseWorldOfWarcraftField settings txt val = do
  worldOfWarcraft <- parseWorldOfWarcraft settings val
  field <- worldOfWarcraft .:? txt .!= mempty
  pure field

parseWorldOfWarcraftFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseWorldOfWarcraftFields settings txts val = do
  worldOfWarcraft <- parseWorldOfWarcraft settings val
  helper worldOfWarcraft txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "worldOfWarcraft" "hero")

$(genProvider "worldOfWarcraft" "hero")

$(genParser "worldOfWarcraft" "quotes")

$(genProvider "worldOfWarcraft" "quotes")
