{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.SuperSmashBros where

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

parseSuperSmashBros :: FromJSON a => FakerSettings -> Value -> Parser a
parseSuperSmashBros settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  games <- faker .: "games"
  superSmashBros <- games .: "super_smash_bros"
  pure superSmashBros
parseSuperSmashBros settings val =
  fail $ "expected Object, but got " <> (show val)

parseSuperSmashBrosField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseSuperSmashBrosField settings txt val = do
  superSmashBros <- parseSuperSmashBros settings val
  field <- superSmashBros .:? txt .!= mempty
  pure field

parseSuperSmashBrosFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseSuperSmashBrosFields settings txts val = do
  superSmashBros <- parseSuperSmashBros settings val
  helper superSmashBros txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "superSmashBros" "fighter")

$(genProvider "superSmashBros" "fighter")

$(genParser "superSmashBros" "stage")

$(genProvider "superSmashBros" "stage")
