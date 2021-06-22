{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Science where

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

parseScience :: FromJSON a => FakerSettings -> Value -> Parser a
parseScience settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  science <- faker .: "science"
  pure science
parseScience settings val = fail $ "expected Object, but got " <> (show val)

parseScienceField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseScienceField settings txt val = do
  science <- parseScience settings val
  field <- science .:? txt .!= mempty
  pure field

parseScienceFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseScienceFields settings txts val = do
  science <- parseScience settings val
  helper science txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "science" "element")

$(genProvider "science" "element")

$(genParser "science" "element_symbol")

$(genProvider "science" "element_symbol")

$(genParser "science" "scientist")

$(genProvider "science" "scientist")

$(genParser "science" "element_state")

$(genProvider "science" "element_state")

$(genParser "science" "element_subcategory")

$(genProvider "science" "element_subcategory")
