{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.BackToTheFuture where

import Config
import Control.Monad.Catch
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH


parseBackToTheFuture :: FromJSON a => FakerSettings -> Value -> Parser a
parseBackToTheFuture settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  backToTheFuture <- faker .: "back_to_the_future"
  pure backToTheFuture
parseBackToTheFuture settings val =
  fail $ "expected Object, but got " <> (show val)

parseBackToTheFutureField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseBackToTheFutureField settings txt val = do
  backToTheFuture <- parseBackToTheFuture settings val
  field <- backToTheFuture .:? txt .!= mempty
  pure field

parseBackToTheFutureFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseBackToTheFutureFields settings txts val = do
  backToTheFuture <- parseBackToTheFuture settings val
  helper backToTheFuture txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "backToTheFuture" "dates")

$(genProvider "backToTheFuture" "dates")

$(genParser "backToTheFuture" "characters")

$(genProvider "backToTheFuture" "characters")

$(genParser "backToTheFuture" "quotes")

$(genProvider "backToTheFuture" "quotes")
