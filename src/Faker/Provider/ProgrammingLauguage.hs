{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.ProgrammingLauguage where

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

parseProgrammingLauguage :: FromJSON a => FakerSettings -> Value -> Parser a
parseProgrammingLauguage settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  programmingLauguage <- faker .: "programming_language"
  pure programmingLauguage
parseProgrammingLauguage settings val = fail $ "expected Object, but got " <> (show val)

parseProgrammingLauguageField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseProgrammingLauguageField settings txt val = do
  programmingLauguage <- parseProgrammingLauguage settings val
  field <- programmingLauguage .:? txt .!= mempty
  pure field

parseProgrammingLauguageFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseProgrammingLauguageFields settings txts val = do
  programmingLauguage <- parseProgrammingLauguage settings val
  helper programmingLauguage txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "programmingLauguage" "name")

$(genProvider "programmingLauguage" "name")


$(genParser "programmingLauguage" "creator")

$(genProvider "programmingLauguage" "creator")











