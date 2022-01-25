{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.SwordArtOnline where

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

parseSwordArtOnline :: FromJSON a => FakerSettings -> Value -> Parser a
parseSwordArtOnline settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  swordArtOnline <- faker .: "sword_art_online"
  pure swordArtOnline
parseSwordArtOnline settings val =
  fail $ "expected Object, but got " <> (show val)

parseSwordArtOnlineField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseSwordArtOnlineField settings txt val = do
  swordArtOnline <- parseSwordArtOnline settings val
  field <- swordArtOnline .:? txt .!= mempty
  pure field

parseSwordArtOnlineFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseSwordArtOnlineFields settings txts val = do
  swordArtOnline <- parseSwordArtOnline settings val
  helper swordArtOnline txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "swordArtOnline" "real_name")

$(genProvider "swordArtOnline" "real_name")

$(genParser "swordArtOnline" "game_name")

$(genProvider "swordArtOnline" "game_name")

$(genParser "swordArtOnline" "location")

$(genProvider "swordArtOnline" "location")

$(genParser "swordArtOnline" "item")

$(genProvider "swordArtOnline" "item")
