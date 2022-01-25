{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.ParksAndRec where

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

parseParksAndRec :: FromJSON a => FakerSettings -> Value -> Parser a
parseParksAndRec settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  parksAndRec <- faker .: "parks_and_rec"
  pure parksAndRec
parseParksAndRec settings val = fail $ "expected Object, but got " <> (show val)

parseParksAndRecField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseParksAndRecField settings txt val = do
  parksAndRec <- parseParksAndRec settings val
  field <- parksAndRec .:? txt .!= mempty
  pure field

parseParksAndRecFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseParksAndRecFields settings txts val = do
  parksAndRec <- parseParksAndRec settings val
  helper parksAndRec txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "parksAndRec" "characters")

$(genProvider "parksAndRec" "characters")

$(genParser "parksAndRec" "cities")

$(genProvider "parksAndRec" "cities")
