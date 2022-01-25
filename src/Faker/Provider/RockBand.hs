{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.RockBand where

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

parseRockBand :: FromJSON a => FakerSettings -> Value -> Parser a
parseRockBand settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  rockBand <- faker .: "rock_band"
  pure rockBand
parseRockBand settings val = fail $ "expected Object, but got " <> (show val)

parseRockBandField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseRockBandField settings txt val = do
  rockBand <- parseRockBand settings val
  field <- rockBand .:? txt .!= mempty
  pure field

parseRockBandFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseRockBandFields settings txts val = do
  rockBand <- parseRockBand settings val
  helper rockBand txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "rockBand" "name")

$(genProvider "rockBand" "name")

$(genParser "rockBand" "song")

$(genProvider "rockBand" "song")
