{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Horse where

import Config
import Control.Monad.Catch
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseHorse :: FromJSON a => FakerSettings -> Value -> Parser a
parseHorse settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  creature <- faker .: "creature"
  horse <- creature .: "horse"
  pure horse
parseHorse settings val = fail $ "expected Object, but got " <> (show val)

parseHorseField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseHorseField settings txt val = do
  horse <- parseHorse settings val
  field <- horse .:? txt .!= mempty
  pure field

parseHorseFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseHorseFields settings txts val = do
  horse <- parseHorse settings val
  helper horse txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "horse" "name")

$(genProvider "horse" "name")

$(genParser "horse" "breed")

$(genProvider "horse" "breed")
