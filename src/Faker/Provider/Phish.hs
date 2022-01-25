{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Phish where

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

parsePhish :: FromJSON a => FakerSettings -> Value -> Parser a
parsePhish settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  phish <- faker .: "phish"
  pure phish
parsePhish settings val = fail $ "expected Object, but got " <> (show val)

parsePhishField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parsePhishField settings txt val = do
  phish <- parsePhish settings val
  field <- phish .:? txt .!= mempty
  pure field

parsePhishFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parsePhishFields settings txts val = do
  phish <- parsePhish settings val
  helper phish txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "phish" "songs")

$(genProvider "phish" "songs")

$(genParser "phish" "musicians")

$(genProvider "phish" "musicians")

$(genParser "phish" "albums")

$(genProvider "phish" "albums")
