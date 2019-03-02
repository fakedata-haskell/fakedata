{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Source where

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

parseSource :: FromJSON a => FakerSettings -> Value -> Parser a
parseSource settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  source <- faker .: "source"
  pure source
parseSource settings val = fail $ "expected Object, but got " <> (show val)

parseSourceField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseSourceField settings txt val = do
  source <- parseSource settings val
  field <- source .:? txt .!= mempty
  pure field

parseSourceFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseSourceFields settings txts val = do
  source <- parseSource settings val
  helper source txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParsers "source" ["hello_world", "ruby"])

$(genProviders "source" ["hello_world", "ruby"])

$(genParsers "source" ["hello_world", "javascript"])

$(genProviders "source" ["hello_world", "javascript"])

$(genParsers "source" ["print", "ruby"])

$(genProviders "source" ["print", "ruby"])

$(genParsers "source" ["print", "javascript"])

$(genProviders "source" ["print", "javascript"])

$(genParsers "source" ["print_1_to_10", "ruby"])

$(genProviders "source" ["print_1_to_10", "ruby"])

$(genParsers "source" ["print_1_to_10", "javascript"])

$(genProviders "source" ["print_1_to_10", "javascript"])
