{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.SiliconValley where

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
import qualified Data.Aeson.Key as K

parseSiliconValley :: FromJSON a => FakerSettings -> Value -> Parser a
parseSiliconValley settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  siliconValley <- faker .: "silicon_valley"
  pure siliconValley
parseSiliconValley settings val =
  fail $ "expected Object, but got " <> (show val)

parseSiliconValleyField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseSiliconValleyField settings txt val = do
  siliconValley <- parseSiliconValley settings val
  field <- siliconValley .:? txt .!= mempty
  pure field

parseSiliconValleyFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseSiliconValleyFields settings txts val = do
  siliconValley <- parseSiliconValley settings val
  helper siliconValley txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "siliconValley" "characters")

$(genProvider "siliconValley" "characters")

$(genParser "siliconValley" "companies")

$(genProvider "siliconValley" "companies")

$(genParser "siliconValley" "quotes")

$(genProvider "siliconValley" "quotes")

$(genParser "siliconValley" "apps")

$(genProvider "siliconValley" "apps")

$(genParser "siliconValley" "inventions")

$(genProvider "siliconValley" "inventions")

$(genParser "siliconValley" "mottos")

$(genProvider "siliconValley" "mottos")

$(genParser "siliconValley" "urls")

$(genProvider "siliconValley" "urls")

$(genParser "siliconValley" "email")

$(genProvider "siliconValley" "email")
