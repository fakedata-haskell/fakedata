{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Football where

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
import qualified Data.Aeson.Key as K

parseFootball :: FromJSON a => FakerSettings -> Value -> Parser a
parseFootball settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  football <- faker .: "football"
  pure football
parseFootball settings val = fail $ "expected Object, but got " <> (show val)

parseFootballField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseFootballField settings txt val = do
  football <- parseFootball settings val
  field <- football .:? txt .!= mempty
  pure field

parseFootballFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseFootballFields settings txts val = do
  football <- parseFootball settings val
  helper football txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "football" "teams")

$(genProvider "football" "teams")

$(genParser "football" "players")

$(genProvider "football" "players")

$(genParser "football" "coaches")

$(genProvider "football" "coaches")

$(genParser "football" "competitions")

$(genProvider "football" "competitions")

$(genParser "football" "positions")

$(genProvider "football" "positions")
