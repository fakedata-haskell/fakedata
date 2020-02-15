{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Gender where

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

parseGender :: FromJSON a => FakerSettings -> Value -> Parser a
parseGender settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  gender <- faker .: "gender"
  pure gender
parseGender settings val = fail $ "expected Object, but got " <> (show val)

parseGenderField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseGenderField settings txt val = do
  gender <- parseGender settings val
  field <- gender .:? txt .!= mempty
  pure field

parseGenderFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseGenderFields settings txts val = do
  gender <- parseGender settings val
  helper gender txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "gender" "types")

$(genProvider "gender" "types")

$(genParser "gender" "binary_types")

$(genProvider "gender" "binary_types")

$(genParser "gender" "short_binary_types")

$(genProvider "gender" "short_binary_types")

