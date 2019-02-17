{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.ElderScrolls where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseElderScrolls :: FromJSON a => FakerSettings -> Value -> Parser a
parseElderScrolls settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  games <- faker .: "games"
  elderScrolls <- games .: "elder_scrolls"
  pure elderScrolls
parseElderScrolls settings val =
  fail $ "expected Object, but got " <> (show val)

parseElderScrollsField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseElderScrollsField settings txt val = do
  elderScrolls <- parseElderScrolls settings val
  field <- elderScrolls .:? txt .!= mempty
  pure field

parseElderScrollsFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseElderScrollsFields settings txts val = do
  elderScrolls <- parseElderScrolls settings val
  helper elderScrolls txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "elderScrolls" "race")

$(genProvider "elderScrolls" "race")

$(genParser "elderScrolls" "creature")

$(genProvider "elderScrolls" "creature")

$(genParser "elderScrolls" "region")

$(genProvider "elderScrolls" "region")

$(genParser "elderScrolls" "dragon")

$(genProvider "elderScrolls" "dragon")

$(genParser "elderScrolls" "city")

$(genProvider "elderScrolls" "city")

$(genParser "elderScrolls" "first_name")

$(genProvider "elderScrolls" "first_name")

$(genParser "elderScrolls" "last_name")

$(genProvider "elderScrolls" "last_name")
