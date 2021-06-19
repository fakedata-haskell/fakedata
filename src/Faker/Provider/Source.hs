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

$(genProvidersSingle "source" ["hello_world", "ruby"])

$(genParsers "source" ["hello_world", "javascript"])

$(genProvidersSingle "source" ["hello_world", "javascript"])

$(genParsers "source" ["hello_world", "c"])

$(genProvidersSingle "source" ["hello_world", "c"])

$(genParsers "source" ["hello_world", "php"])

$(genProvidersSingle "source" ["hello_world", "php"])

$(genParsers "source" ["hello_world", "python"])

$(genProvidersSingle "source" ["hello_world", "python"])

$(genParsers "source" ["hello_world", "java"])

$(genProvidersSingle "source" ["hello_world", "java"])

$(genParsers "source" ["hello_world", "elixir"])

$(genProvidersSingle "source" ["hello_world", "elixir"])

$(genParsers "source" ["print", "ruby"])

$(genProvidersSingle "source" ["print", "ruby"])

$(genParsers "source" ["print", "javascript"])

$(genProvidersSingle "source" ["print", "javascript"])

$(genParsers "source" ["print", "c"])

$(genProvidersSingle "source" ["print", "c"])

$(genParsers "source" ["print", "php"])

$(genProvidersSingle "source" ["print", "php"])

$(genParsers "source" ["print", "python"])

$(genProvidersSingle "source" ["print", "python"])

$(genParsers "source" ["print", "java"])

$(genProvidersSingle "source" ["print", "java"])

$(genParsers "source" ["print", "elixir"])

$(genProvidersSingle "source" ["print", "elixir"])

$(genParsers "source" ["print_1_to_10", "ruby"])

$(genProvidersSingle "source" ["print_1_to_10", "ruby"])

$(genParsers "source" ["print_1_to_10", "javascript"])

$(genProvidersSingle "source" ["print_1_to_10", "javascript"])

$(genParsers "source" ["print_1_to_10", "c"])

$(genProvidersSingle "source" ["print_1_to_10", "c"])

$(genParsers "source" ["print_1_to_10", "php"])

$(genProvidersSingle "source" ["print_1_to_10", "php"])

$(genParsers "source" ["print_1_to_10", "python"])

$(genProvidersSingle "source" ["print_1_to_10", "python"])

$(genParsers "source" ["print_1_to_10", "java"])

$(genProvidersSingle "source" ["print_1_to_10", "java"])

$(genParsers "source" ["print_1_to_10", "elixir"])

$(genProvidersSingle "source" ["print_1_to_10", "elixir"])
