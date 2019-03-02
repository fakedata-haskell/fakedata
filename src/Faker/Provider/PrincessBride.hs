{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.PrincessBride where

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

parsePrincessBride :: FromJSON a => FakerSettings -> Value -> Parser a
parsePrincessBride settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  princessBride <- faker .: "princess_bride"
  pure princessBride
parsePrincessBride settings val =
  fail $ "expected Object, but got " <> (show val)

parsePrincessBrideField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parsePrincessBrideField settings txt val = do
  princessBride <- parsePrincessBride settings val
  field <- princessBride .:? txt .!= mempty
  pure field

parsePrincessBrideFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parsePrincessBrideFields settings txts val = do
  princessBride <- parsePrincessBride settings val
  helper princessBride txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "princessBride" "characters")

$(genProvider "princessBride" "characters")

$(genParser "princessBride" "quotes")

$(genProvider "princessBride" "quotes")
