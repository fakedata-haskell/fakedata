{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.ElectricalComponents where

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

parseElectricalComponents :: FromJSON a => FakerSettings -> Value -> Parser a
parseElectricalComponents settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  electricalComponents <- faker .: "electrical_components"
  pure electricalComponents
parseElectricalComponents settings val =
  fail $ "expected Object, but got " <> (show val)

parseElectricalComponentsField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseElectricalComponentsField settings txt val = do
  electricalComponents <- parseElectricalComponents settings val
  field <- electricalComponents .:? txt .!= mempty
  pure field

parseElectricalComponentsFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseElectricalComponentsFields settings txts val = do
  electricalComponents <- parseElectricalComponents settings val
  helper electricalComponents txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "electricalComponents" "active")

$(genProvider "electricalComponents" "active")

$(genParser "electricalComponents" "passive")

$(genProvider "electricalComponents" "passive")

$(genParser "electricalComponents" "electromechanical")

$(genProvider "electricalComponents" "electromechanical")
