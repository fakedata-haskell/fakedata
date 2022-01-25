{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.UmphreysMcgee where

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

parseUmphreysMcgee :: FromJSON a => FakerSettings -> Value -> Parser a
parseUmphreysMcgee settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  umphreysMcgee <- faker .: "umphreys_mcgee"
  pure umphreysMcgee
parseUmphreysMcgee settings val =
  fail $ "expected Object, but got " <> (show val)

parseUmphreysMcgeeField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseUmphreysMcgeeField settings txt val = do
  umphreysMcgee <- parseUmphreysMcgee settings val
  field <- umphreysMcgee .:? txt .!= mempty
  pure field

parseUmphreysMcgeeFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseUmphreysMcgeeFields settings txts val = do
  umphreysMcgee <- parseUmphreysMcgee settings val
  helper umphreysMcgee txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "umphreysMcgee" "song")

$(genProvider "umphreysMcgee" "song")
