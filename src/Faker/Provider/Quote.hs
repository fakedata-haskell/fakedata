{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Quote where

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

parseQuote :: FromJSON a => FakerSettings -> Value -> Parser a
parseQuote settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  quote <- faker .: "quote"
  pure quote
parseQuote settings val = fail $ "expected Object, but got " <> (show val)

parseQuoteField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseQuoteField settings txt val = do
  quote <- parseQuote settings val
  field <- quote .:? txt .!= mempty
  pure field

parseQuoteFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseQuoteFields settings txts val = do
  quote <- parseQuote settings val
  helper quote txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "quote" "famous_last_words")

$(genProvider "quote" "famous_last_words")

$(genParser "quote" "matz")

$(genProvider "quote" "matz")

$(genParser "quote" "most_interesting_man_in_the_world")

$(genProvider "quote" "most_interesting_man_in_the_world")

$(genParser "quote" "robin")

$(genProvider "quote" "robin")

$(genParser "quote" "singular_siegler")

$(genProvider "quote" "singular_siegler")

$(genParser "quote" "yoda")

$(genProvider "quote" "yoda")
