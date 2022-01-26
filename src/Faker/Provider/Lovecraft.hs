{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Lovecraft where

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


parseLovecraft :: FromJSON a => FakerSettings -> Value -> Parser a
parseLovecraft settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  lovecraft <- faker .: "lovecraft"
  pure lovecraft
parseLovecraft settings val = fail $ "expected Object, but got " <> (show val)

parseLovecraftField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseLovecraftField settings txt val = do
  lovecraft <- parseLovecraft settings val
  field <- lovecraft .:? txt .!= mempty
  pure field

parseLovecraftFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseLovecraftFields settings txts val = do
  lovecraft <- parseLovecraft settings val
  helper lovecraft txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "lovecraft" "fhtagn")

$(genProvider "lovecraft" "fhtagn")

$(genParser "lovecraft" "deity")

$(genProvider "lovecraft" "deity")

$(genParser "lovecraft" "location")

$(genProvider "lovecraft" "location")

$(genParser "lovecraft" "tome")

$(genProvider "lovecraft" "tome")

$(genParser "lovecraft" "words")

$(genProvider "lovecraft" "words")
