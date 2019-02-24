{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Witcher where

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

parseWitcher :: FromJSON a => FakerSettings -> Value -> Parser a
parseWitcher settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  witcher <- faker .: "witcher"
  pure witcher
parseWitcher settings val = fail $ "expected Object, but got " <> (show val)

parseWitcherField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseWitcherField settings txt val = do
  witcher <- parseWitcher settings val
  field <- witcher .:? txt .!= mempty
  pure field

parseWitcherFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseWitcherFields settings txts val = do
  witcher <- parseWitcher settings val
  helper witcher txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "witcher" "characters")

$(genProvider "witcher" "characters")


$(genParser "witcher" "witchers")

$(genProvider "witcher" "witchers")


$(genParser "witcher" "schools")

$(genProvider "witcher" "schools")


$(genParser "witcher" "locations")

$(genProvider "witcher" "locations")


$(genParser "witcher" "quotes")

$(genProvider "witcher" "quotes")


$(genParser "witcher" "monsters")

$(genProvider "witcher" "monsters")











