{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Myst where

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

parseMyst :: FromJSON a => FakerSettings -> Value -> Parser a
parseMyst settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  games <- faker .: "games"
  myst <- games .: "myst"
  pure myst
parseMyst settings val = fail $ "expected Object, but got " <> (show val)

parseMystField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseMystField settings txt val = do
  myst <- parseMyst settings val
  field <- myst .:? txt .!= mempty
  pure field

parseMystFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseMystFields settings txts val = do
  myst <- parseMyst settings val
  helper myst txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "myst" "games")

$(genProvider "myst" "games")

$(genParser "myst" "creatures")

$(genProvider "myst" "creatures")

$(genParser "myst" "characters")

$(genProvider "myst" "characters")

$(genParser "myst" "ages")

$(genProvider "myst" "ages")

$(genParser "myst" "quotes")

$(genProvider "myst" "quotes")
