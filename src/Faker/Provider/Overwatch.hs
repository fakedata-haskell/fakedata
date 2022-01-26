{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Overwatch where

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


parseOverwatch :: FromJSON a => FakerSettings -> Value -> Parser a
parseOverwatch settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  games <- faker .: "games"
  overwatch <- games .: "overwatch"
  pure overwatch
parseOverwatch settings val = fail $ "expected Object, but got " <> (show val)

parseOverwatchField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseOverwatchField settings txt val = do
  overwatch <- parseOverwatch settings val
  field <- overwatch .:? txt .!= mempty
  pure field

parseOverwatchFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseOverwatchFields settings txts val = do
  overwatch <- parseOverwatch settings val
  helper overwatch txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "overwatch" "heroes")

$(genProvider "overwatch" "heroes")

$(genParser "overwatch" "locations")

$(genProvider "overwatch" "locations")

$(genParser "overwatch" "quotes")

$(genProvider "overwatch" "quotes")
