{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.VentureBros where

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

parseVentureBros :: FromJSON a => FakerSettings -> Value -> Parser a
parseVentureBros settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  ventureBros <- faker .: "venture_bros"
  pure ventureBros
parseVentureBros settings val = fail $ "expected Object, but got " <> (show val)

parseVentureBrosField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseVentureBrosField settings txt val = do
  ventureBros <- parseVentureBros settings val
  field <- ventureBros .:? txt .!= mempty
  pure field

parseVentureBrosFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseVentureBrosFields settings txts val = do
  ventureBros <- parseVentureBros settings val
  helper ventureBros txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "ventureBros" "character")

$(genProvider "ventureBros" "character")


$(genParser "ventureBros" "organization")

$(genProvider "ventureBros" "organization")


$(genParser "ventureBros" "vehicle")

$(genProvider "ventureBros" "vehicle")


$(genParser "ventureBros" "quote")

$(genProvider "ventureBros" "quote")











