{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Computer where

import Config
import Control.Monad.Catch
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH
import qualified Data.Aeson.Key as K

parseComputer :: FromJSON a => FakerSettings -> Value -> Parser a
parseComputer settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  computer <- faker .: "computer"
  pure computer
parseComputer settings val = fail $ "expected Object, but got " <> (show val)

parseComputerField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseComputerField settings txt val = do
  computer <- parseComputer settings val
  field <- computer .:? txt .!= mempty
  pure field

parseComputerFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseComputerFields settings txts val = do
  computer <- parseComputer settings val
  helper computer txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "computer" "type")

$(genProvider "computer" "type")

$(genParser "computer" "platform")

$(genProvider "computer" "platform")

$(genParsers "computer" ["os", "linux"])

$(genProviders "computer" ["os", "linux"])

$(genParsers "computer" ["os", "macos"])

$(genProviders "computer" ["os", "macos"])

$(genParsers "computer" ["os", "windows"])

$(genProviders "computer" ["os", "windows"])
