{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Esport where

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

parseEsport :: FromJSON a => FakerSettings -> Value -> Parser a
parseEsport settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  esport <- faker .: "esport"
  pure esport
parseEsport settings val = fail $ "expected Object, but got " <> (show val)

parseEsportField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseEsportField settings txt val = do
  esport <- parseEsport settings val
  field <- esport .:? txt .!= mempty
  pure field

parseEsportFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseEsportFields settings txts val = do
  esport <- parseEsport settings val
  helper esport txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "esport" "players")

$(genProvider "esport" "players")

$(genParser "esport" "teams")

$(genProvider "esport" "teams")

$(genParser "esport" "events")

$(genProvider "esport" "events")

$(genParser "esport" "leagues")

$(genProvider "esport" "leagues")

$(genParser "esport" "games")

$(genProvider "esport" "games")
