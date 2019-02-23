{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.SourthPark where

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

parseSourthPark :: FromJSON a => FakerSettings -> Value -> Parser a
parseSourthPark settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  sourthPark <- faker .: "south_park"
  pure sourthPark
parseSourthPark settings val = fail $ "expected Object, but got " <> (show val)

parseSourthParkField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseSourthParkField settings txt val = do
  sourthPark <- parseSourthPark settings val
  field <- sourthPark .:? txt .!= mempty
  pure field

parseSourthParkFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseSourthParkFields settings txts val = do
  sourthPark <- parseSourthPark settings val
  helper sourthPark txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "sourthPark" "characters")

$(genProvider "sourthPark" "characters")


$(genParser "sourthPark" "quotes")

$(genProvider "sourthPark" "quotes")











