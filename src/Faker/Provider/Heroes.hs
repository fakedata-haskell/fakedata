{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Heroes where

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

parseHeroes :: FromJSON a => FakerSettings -> Value -> Parser a
parseHeroes settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  heroes <- faker .: "heroes"
  pure heroes
parseHeroes settings val = fail $ "expected Object, but got " <> (show val)

parseHeroesField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseHeroesField settings txt val = do
  heroes <- parseHeroes settings val
  field <- heroes .:? txt .!= mempty
  pure field

parseHeroesFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseHeroesFields settings txts val = do
  heroes <- parseHeroes settings val
  helper heroes txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "heroes" "names")

$(genProvider "heroes" "names")

$(genParser "heroes" "specialties")

$(genProvider "heroes" "specialties")

$(genParser "heroes" "klasses")

$(genProvider "heroes" "klasses")

$(genParser "heroes" "artifacts")

$(genProvider "heroes" "artifacts")
