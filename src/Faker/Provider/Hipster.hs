{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Hipster where

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

parseHipster :: FromJSON a => FakerSettings -> Value -> Parser a
parseHipster settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  hipster <- faker .: "hipster"
  pure hipster
parseHipster settings val = fail $ "expected Object, but got " <> (show val)

parseHipsterField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseHipsterField settings txt val = do
  hipster <- parseHipster settings val
  field <- hipster .:? txt .!= mempty
  pure field

parseHipsterFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseHipsterFields settings txts val = do
  hipster <- parseHipster settings val
  helper hipster txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "hipster" "words")

$(genProvider "hipster" "words")
