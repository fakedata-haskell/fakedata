{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Pokemon where

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

parsePokemon :: FromJSON a => FakerSettings -> Value -> Parser a
parsePokemon settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  games <- faker .: "games"
  pokemon <- games .: "pokemon"
  pure pokemon
parsePokemon settings val = fail $ "expected Object, but got " <> (show val)

parsePokemonField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parsePokemonField settings txt val = do
  pokemon <- parsePokemon settings val
  field <- pokemon .:? txt .!= mempty
  pure field

parsePokemonFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parsePokemonFields settings txts val = do
  pokemon <- parsePokemon settings val
  helper pokemon txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "pokemon" "names")

$(genProvider "pokemon" "names")

$(genParser "pokemon" "locations")

$(genProvider "pokemon" "locations")

$(genParser "pokemon" "moves")

$(genProvider "pokemon" "moves")
