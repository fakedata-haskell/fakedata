{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.FreshPrinceOfBelAir where

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

parseFreshPrinceOfBelAir :: FromJSON a => FakerSettings -> Value -> Parser a
parseFreshPrinceOfBelAir settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  freshPrinceOfBelAir <- faker .: "the_fresh_prince_of_bel_air"
  pure freshPrinceOfBelAir
parseFreshPrinceOfBelAir settings val =
  fail $ "expected Object, but got " <> (show val)

parseFreshPrinceOfBelAirField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseFreshPrinceOfBelAirField settings txt val = do
  freshPrinceOfBelAir <- parseFreshPrinceOfBelAir settings val
  field <- freshPrinceOfBelAir .:? txt .!= mempty
  pure field

parseFreshPrinceOfBelAirFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseFreshPrinceOfBelAirFields settings txts val = do
  freshPrinceOfBelAir <- parseFreshPrinceOfBelAir settings val
  helper freshPrinceOfBelAir txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "freshPrinceOfBelAir" "characters")

$(genProvider "freshPrinceOfBelAir" "characters")

$(genParser "freshPrinceOfBelAir" "actors")

$(genProvider "freshPrinceOfBelAir" "actors")

$(genParser "freshPrinceOfBelAir" "quotes")

$(genProvider "freshPrinceOfBelAir" "quotes")
