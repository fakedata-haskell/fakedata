{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Zelda where

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

parseZelda :: FromJSON a => FakerSettings -> Value -> Parser a
parseZelda settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  games <- faker .: "games"
  zelda <- games .: "zelda"
  pure zelda
parseZelda settings val = fail $ "expected Object, but got " <> (show val)

parseZeldaField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseZeldaField settings txt val = do
  zelda <- parseZelda settings val
  field <- zelda .:? txt .!= mempty
  pure field

parseZeldaFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseZeldaFields settings txts val = do
  zelda <- parseZelda settings val
  helper zelda txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "zelda" "games")

$(genProvider "zelda" "games")

$(genParser "zelda" "characters")

$(genProvider "zelda" "characters")

$(genParser "zelda" "locations")

$(genProvider "zelda" "locations")

$(genParser "zelda" "items")

$(genProvider "zelda" "items")
