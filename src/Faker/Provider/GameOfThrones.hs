{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.GameOfThrones where

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

parseGameOfThrones :: FromJSON a => FakerSettings -> Value -> Parser a
parseGameOfThrones settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  gameOfThrones <- faker .: "game_of_thrones"
  pure gameOfThrones
parseGameOfThrones settings val =
  fail $ "expected Object, but got " <> (show val)

parseGameOfThronesField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseGameOfThronesField settings txt val = do
  gameOfThrones <- parseGameOfThrones settings val
  field <- gameOfThrones .:? txt .!= mempty
  pure field

parseGameOfThronesFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseGameOfThronesFields settings txts val = do
  gameOfThrones <- parseGameOfThrones settings val
  helper gameOfThrones txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "gameOfThrones" "characters")

$(genProvider "gameOfThrones" "characters")

$(genParser "gameOfThrones" "houses")

$(genProvider "gameOfThrones" "houses")

$(genParser "gameOfThrones" "cities")

$(genProvider "gameOfThrones" "cities")

$(genParser "gameOfThrones" "quotes")

$(genProvider "gameOfThrones" "quotes")

$(genParser "gameOfThrones" "dragons")

$(genProvider "gameOfThrones" "dragons")
