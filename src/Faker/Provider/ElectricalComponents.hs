{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.ElectricalComponents where

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

parseElectricalComponents :: FromJSON a => FakerSettings -> Value -> Parser a
parseElectricalComponents settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  electricalComponents <- faker .: "electrical_components"
  pure electricalComponents
parseElectricalComponents settings val =
  fail $ "expected Object, but got " <> (show val)

parseElectricalComponentsField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseElectricalComponentsField settings txt val = do
  electricalComponents <- parseElectricalComponents settings val
  field <- electricalComponents .:? txt .!= mempty
  pure field

parseElectricalComponentsFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseElectricalComponentsFields settings txts val = do
  electricalComponents <- parseElectricalComponents settings val
  helper electricalComponents txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
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
