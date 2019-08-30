{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.GratefulDead where

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

parseGratefulDead :: FromJSON a => FakerSettings -> Value -> Parser a
parseGratefulDead settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  gratefulDead <- faker .: "grateful_dead"
  pure gratefulDead
parseGratefulDead settings val =
  fail $ "expected Object, but got " <> (show val)

parseGratefulDeadField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseGratefulDeadField settings txt val = do
  gratefulDead <- parseGratefulDead settings val
  field <- gratefulDead .:? txt .!= mempty
  pure field

parseGratefulDeadFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseGratefulDeadFields settings txts val = do
  gratefulDead <- parseGratefulDead settings val
  helper gratefulDead txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "gratefulDead" "players")

$(genProvider "gratefulDead" "players")

$(genParser "gratefulDead" "songs")

$(genProvider "gratefulDead" "songs")
