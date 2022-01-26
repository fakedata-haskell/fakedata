{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Rupaul where

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


parseRupaul :: FromJSON a => FakerSettings -> Value -> Parser a
parseRupaul settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  rupaul <- faker .: "rupaul"
  pure rupaul
parseRupaul settings val = fail $ "expected Object, but got " <> (show val)

parseRupaulField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseRupaulField settings txt val = do
  rupaul <- parseRupaul settings val
  field <- rupaul .:? txt .!= mempty
  pure field

parseRupaulFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseRupaulFields settings txts val = do
  rupaul <- parseRupaul settings val
  helper rupaul txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "rupaul" "queens")

$(genProvider "rupaul" "queens")

$(genParser "rupaul" "quotes")

$(genProvider "rupaul" "quotes")
