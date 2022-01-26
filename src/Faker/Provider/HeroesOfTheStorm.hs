{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.HeroesOfTheStorm where

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


parseHeroesOfTheStorm :: FromJSON a => FakerSettings -> Value -> Parser a
parseHeroesOfTheStorm settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  heroesOfTheStorm <- faker .: "heroes_of_the_storm"
  pure heroesOfTheStorm
parseHeroesOfTheStorm settings val =
  fail $ "expected Object, but got " <> (show val)

parseHeroesOfTheStormField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseHeroesOfTheStormField settings txt val = do
  heroesOfTheStorm <- parseHeroesOfTheStorm settings val
  field <- heroesOfTheStorm .:? txt .!= mempty
  pure field

parseHeroesOfTheStormFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseHeroesOfTheStormFields settings txts val = do
  heroesOfTheStorm <- parseHeroesOfTheStorm settings val
  helper heroesOfTheStorm txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "heroesOfTheStorm" "battlegrounds")

$(genProvider "heroesOfTheStorm" "battlegrounds")

$(genParser "heroesOfTheStorm" "class_names")

$(genProvider "heroesOfTheStorm" "class_names")

$(genParser "heroesOfTheStorm" "heroes")

$(genProvider "heroesOfTheStorm" "heroes")

$(genParser "heroesOfTheStorm" "quotes")

$(genProvider "heroesOfTheStorm" "quotes")
