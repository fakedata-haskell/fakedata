{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.RickAndMorty where

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

parseRickAndMorty :: FromJSON a => FakerSettings -> Value -> Parser a
parseRickAndMorty settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  rickAndMorty <- faker .: "rick_and_morty"
  pure rickAndMorty
parseRickAndMorty settings val =
  fail $ "expected Object, but got " <> (show val)

parseRickAndMortyField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseRickAndMortyField settings txt val = do
  rickAndMorty <- parseRickAndMorty settings val
  field <- rickAndMorty .:? txt .!= mempty
  pure field

parseRickAndMortyFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseRickAndMortyFields settings txts val = do
  rickAndMorty <- parseRickAndMorty settings val
  helper rickAndMorty txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "rickAndMorty" "characters")

$(genProvider "rickAndMorty" "characters")

$(genParser "rickAndMorty" "locations")

$(genProvider "rickAndMorty" "locations")

$(genParser "rickAndMorty" "quotes")

$(genProvider "rickAndMorty" "quotes")
