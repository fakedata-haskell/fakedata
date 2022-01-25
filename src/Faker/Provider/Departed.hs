{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Departed where

import Config
import Control.Monad.Catch
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Monoid ((<>))
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH
import qualified Data.Aeson.Key as K

parseDeparted :: FromJSON a => FakerSettings -> Value -> Parser a
parseDeparted settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  departed <- faker .: "departed"
  pure departed
parseDeparted settings val = fail $ "expected Object, but got " <> (show val)

parseDepartedField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseDepartedField settings txt val = do
  departed <- parseDeparted settings val
  field <- departed .:? txt .!= mempty
  pure field

parseDepartedFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseDepartedFields settings txts val = do
  departed <- parseDeparted settings val
  helper departed txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "departed" "actors")

$(genProvider "departed" "actors")


$(genParser "departed" "characters")

$(genProvider "departed" "characters")


$(genParser "departed" "quotes")

$(genProvider "departed" "quotes")
