{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.TheThickOfIt where

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


parseTheThickOfIt :: FromJSON a => FakerSettings -> Value -> Parser a
parseTheThickOfIt settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  theThickOfIt <- faker .: "the_thick_of_it"
  pure theThickOfIt
parseTheThickOfIt settings val =
  fail $ "expected Object, but got " <> (show val)

parseTheThickOfItField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseTheThickOfItField settings txt val = do
  theThickOfIt <- parseTheThickOfIt settings val
  field <- theThickOfIt .:? txt .!= mempty
  pure field

parseTheThickOfItFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseTheThickOfItFields settings txts val = do
  theThickOfIt <- parseTheThickOfIt settings val
  helper theThickOfIt txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "theThickOfIt" "characters")

$(genProvider "theThickOfIt" "characters")

$(genParser "theThickOfIt" "positions")

$(genProvider "theThickOfIt" "positions")

$(genParser "theThickOfIt" "departments")

$(genProvider "theThickOfIt" "departments")
