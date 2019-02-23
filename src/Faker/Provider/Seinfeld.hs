{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Seinfeld where

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

parseSeinfeld :: FromJSON a => FakerSettings -> Value -> Parser a
parseSeinfeld settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  seinfeld <- faker .: "seinfeld"
  pure seinfeld
parseSeinfeld settings val = fail $ "expected Object, but got " <> (show val)

parseSeinfeldField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseSeinfeldField settings txt val = do
  seinfeld <- parseSeinfeld settings val
  field <- seinfeld .:? txt .!= mempty
  pure field

parseSeinfeldFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseSeinfeldFields settings txts val = do
  seinfeld <- parseSeinfeld settings val
  helper seinfeld txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "seinfeld" "character")

$(genProvider "seinfeld" "character")


$(genParser "seinfeld" "quote")

$(genProvider "seinfeld" "quote")


$(genParser "seinfeld" "business")

$(genProvider "seinfeld" "business")











