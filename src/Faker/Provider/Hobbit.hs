{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Hobbit where

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

parseHobbit :: FromJSON a => FakerSettings -> Value -> Parser a
parseHobbit settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  hobbit <- faker .: "hobbit"
  pure hobbit
parseHobbit settings val = fail $ "expected Object, but got " <> (show val)

parseHobbitField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseHobbitField settings txt val = do
  hobbit <- parseHobbit settings val
  field <- hobbit .:? txt .!= mempty
  pure field

parseHobbitFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseHobbitFields settings txts val = do
  hobbit <- parseHobbit settings val
  helper hobbit txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "hobbit" "character")

$(genProvider "hobbit" "character")

$(genParser "hobbit" "thorins_company")

$(genProvider "hobbit" "thorins_company")

$(genParser "hobbit" "quote")

$(genProvider "hobbit" "quote")

$(genParser "hobbit" "location")

$(genProvider "hobbit" "location")
