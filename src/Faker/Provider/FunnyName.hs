{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.FunnyName where

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

parseFunnyName :: FromJSON a => FakerSettings -> Value -> Parser a
parseFunnyName settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  funnyName <- faker .: "funny_name"
  pure funnyName
parseFunnyName settings val = fail $ "expected Object, but got " <> (show val)

parseFunnyNameField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseFunnyNameField settings txt val = do
  funnyName <- parseFunnyName settings val
  field <- funnyName .:? txt .!= mempty
  pure field

parseFunnyNameFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseFunnyNameFields settings txts val = do
  funnyName <- parseFunnyName settings val
  helper funnyName txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "funnyName" "name")

$(genProvider "funnyName" "name")
