{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Verbs where

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

parseVerbs :: FromJSON a => FakerSettings -> Value -> Parser a
parseVerbs settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  verbs <- faker .: "verbs"
  pure verbs
parseVerbs settings val = fail $ "expected Object, but got " <> (show val)

parseVerbsField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseVerbsField settings txt val = do
  verbs <- parseVerbs settings val
  field <- verbs .:? txt .!= mempty
  pure field

parseVerbsFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseVerbsFields settings txts val = do
  verbs <- parseVerbs settings val
  helper verbs txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "verbs" "base")

$(genProvider "verbs" "base")


$(genParser "verbs" "past")

$(genProvider "verbs" "past")


$(genParser "verbs" "past_participle")

$(genProvider "verbs" "past_participle")


$(genParser "verbs" "simple_present")

$(genProvider "verbs" "simple_present")


$(genParser "verbs" "ing_form")

$(genProvider "verbs" "ing_form")











