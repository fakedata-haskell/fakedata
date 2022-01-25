{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.TheExpanse where

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

parseTheExpanse :: FromJSON a => FakerSettings -> Value -> Parser a
parseTheExpanse settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  theExpanse <- faker .: "the_expanse"
  pure theExpanse
parseTheExpanse settings val = fail $ "expected Object, but got " <> (show val)

parseTheExpanseField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseTheExpanseField settings txt val = do
  theExpanse <- parseTheExpanse settings val
  field <- theExpanse .:? txt .!= mempty
  pure field

parseTheExpanseFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseTheExpanseFields settings txts val = do
  theExpanse <- parseTheExpanse settings val
  helper theExpanse txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "theExpanse" "characters")

$(genProvider "theExpanse" "characters")

$(genParser "theExpanse" "locations")

$(genProvider "theExpanse" "locations")

$(genParser "theExpanse" "ships")

$(genProvider "theExpanse" "ships")

$(genParser "theExpanse" "quotes")

$(genProvider "theExpanse" "quotes")
