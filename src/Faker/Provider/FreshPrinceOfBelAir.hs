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

parseFreshPrinceOfBelAir :: FromJSON a => FakerSettings -> Value -> Parser a
parseFreshPrinceOfBelAir settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  freshPrinceOfBelAir <- faker .: "the_fresh_prince_of_bel_air"
  pure freshPrinceOfBelAir
parseFreshPrinceOfBelAir settings val =
  fail $ "expected Object, but got " <> (show val)

parseFreshPrinceOfBelAirField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseFreshPrinceOfBelAirField settings txt val = do
  freshPrinceOfBelAir <- parseFreshPrinceOfBelAir settings val
  field <- freshPrinceOfBelAir .:? txt .!= mempty
  pure field

parseFreshPrinceOfBelAirFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseFreshPrinceOfBelAirFields settings txts val = do
  freshPrinceOfBelAir <- parseFreshPrinceOfBelAir settings val
  helper freshPrinceOfBelAir txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "freshPrinceOfBelAir" "characters")

$(genProvider "freshPrinceOfBelAir" "characters")

$(genParser "freshPrinceOfBelAir" "celebrities")

$(genProvider "freshPrinceOfBelAir" "celebrities")

$(genParser "freshPrinceOfBelAir" "quotes")

$(genProvider "freshPrinceOfBelAir" "quotes")
