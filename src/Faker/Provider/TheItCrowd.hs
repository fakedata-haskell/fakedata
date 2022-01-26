{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.TheItCrowd where

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


parseTheItCrowd :: FromJSON a => FakerSettings -> Value -> Parser a
parseTheItCrowd settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  theItCrowd <- faker .: "the_it_crowd"
  pure theItCrowd
parseTheItCrowd settings val = fail $ "expected Object, but got " <> (show val)

parseTheItCrowdField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseTheItCrowdField settings txt val = do
  theItCrowd <- parseTheItCrowd settings val
  field <- theItCrowd .:? txt .!= mempty
  pure field

parseTheItCrowdFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseTheItCrowdFields settings txts val = do
  theItCrowd <- parseTheItCrowd settings val
  helper theItCrowd txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "theItCrowd" "actors")

$(genProvider "theItCrowd" "actors")

$(genParser "theItCrowd" "characters")

$(genProvider "theItCrowd" "characters")

$(genParser "theItCrowd" "emails")

$(genProvider "theItCrowd" "emails")

$(genParser "theItCrowd" "quotes")

$(genProvider "theItCrowd" "quotes")
