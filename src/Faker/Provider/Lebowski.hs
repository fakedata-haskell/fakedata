{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Lebowski where

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

parseLebowski :: FromJSON a => FakerSettings -> Value -> Parser a
parseLebowski settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  lebowski <- faker .: "lebowski"
  pure lebowski
parseLebowski settings val = fail $ "expected Object, but got " <> (show val)

parseLebowskiField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseLebowskiField settings txt val = do
  lebowski <- parseLebowski settings val
  field <- lebowski .:? txt .!= mempty
  pure field

parseLebowskiFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseLebowskiFields settings txts val = do
  lebowski <- parseLebowski settings val
  helper lebowski txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "lebowski" "actors")

$(genProvider "lebowski" "actors")

$(genParser "lebowski" "characters")

$(genProvider "lebowski" "characters")

$(genParser "lebowski" "quotes")

$(genProvider "lebowski" "quotes")
