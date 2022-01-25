{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Simpsons where

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

parseSimpsons :: FromJSON a => FakerSettings -> Value -> Parser a
parseSimpsons settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  simpsons <- faker .: "simpsons"
  pure simpsons
parseSimpsons settings val = fail $ "expected Object, but got " <> (show val)

parseSimpsonsField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseSimpsonsField settings txt val = do
  simpsons <- parseSimpsons settings val
  field <- simpsons .:? txt .!= mempty
  pure field

parseSimpsonsFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseSimpsonsFields settings txts val = do
  simpsons <- parseSimpsons settings val
  helper simpsons txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "simpsons" "characters")

$(genProvider "simpsons" "characters")

$(genParser "simpsons" "locations")

$(genProvider "simpsons" "locations")

$(genParser "simpsons" "quotes")

$(genProvider "simpsons" "quotes")

$(genParser "simpsons" "episode_titles")

$(genProvider "simpsons" "episode_titles")
