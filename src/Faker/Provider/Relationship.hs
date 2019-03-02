{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Relationship where

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

parseRelationship :: FromJSON a => FakerSettings -> Value -> Parser a
parseRelationship settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  relationship <- faker .: "relationship"
  pure relationship
parseRelationship settings val =
  fail $ "expected Object, but got " <> (show val)

parseRelationshipField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseRelationshipField settings txt val = do
  relationship <- parseRelationship settings val
  field <- relationship .:? txt .!= mempty
  pure field

parseRelationshipFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseRelationshipFields settings txts val = do
  relationship <- parseRelationship settings val
  helper relationship txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "relationship" "in_law")

$(genProvider "relationship" "in_law")

$(genParser "relationship" "spouse")

$(genProvider "relationship" "spouse")

$(genParser "relationship" "parent")

$(genProvider "relationship" "parent")

$(genParser "relationship" "sibling")

$(genProvider "relationship" "sibling")

$(genParsers "relationship" ["familial", "direct"])

$(genProviders "relationship" ["familial", "direct"])

$(genParsers "relationship" ["familial", "extended"])

$(genProviders "relationship" ["familial", "extended"])
