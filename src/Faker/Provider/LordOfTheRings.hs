{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.LordOfTheRings where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseLordOfTheRings :: FromJSON a => FakerSettings -> Value -> Parser a
parseLordOfTheRings settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  lordOfTheRings <- faker .: "lord_of_the_rings"
  pure lordOfTheRings
parseLordOfTheRings settings val = fail $ "expected Object, but got " <> (show val)

parseLordOfTheRingsField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseLordOfTheRingsField settings txt val = do
  lordOfTheRings <- parseLordOfTheRings settings val
  field <- lordOfTheRings .:? txt .!= mempty
  pure field

parseLordOfTheRingsFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseLordOfTheRingsFields settings txts val = do
  lordOfTheRings <- parseLordOfTheRings settings val
  helper lordOfTheRings txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "lordOfTheRings" "characters")

$(genProvider "lordOfTheRings" "characters")


$(genParser "lordOfTheRings" "locations")

$(genProvider "lordOfTheRings" "locations")


$(genParser "lordOfTheRings" "quotes")

$(genProvider "lordOfTheRings" "quotes")











