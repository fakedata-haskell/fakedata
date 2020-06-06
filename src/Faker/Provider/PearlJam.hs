{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.PearlJam where

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

parsePearlJam :: FromJSON a => FakerSettings -> Value -> Parser a
parsePearlJam settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  pearlJam <- faker .: "pearl_jam"
  pure pearlJam
parsePearlJam settings val = fail $ "expected Object, but got " <> (show val)

parsePearlJamField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parsePearlJamField settings txt val = do
  pearlJam <- parsePearlJam settings val
  field <- pearlJam .:? txt .!= mempty
  pure field

parsePearlJamFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parsePearlJamFields settings txts val = do
  pearlJam <- parsePearlJam settings val
  helper pearlJam txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "pearl_jam" "musicians")

$(genProvider "pearl_jam" "musicians")

$(genParser "pearl_jam" "albums")

$(genProvider "pearl_jam" "albums")

$(genParser "pearl_jam" "songs")

$(genProvider "pearl_jam" "songs")
