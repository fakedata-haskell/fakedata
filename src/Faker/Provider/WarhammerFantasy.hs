{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.WarhammerFantasy where

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

parseWarhammerFantasy :: FromJSON a => FakerSettings -> Value -> Parser a
parseWarhammerFantasy settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  games <- faker .: "games"
  warhammerFantasy <- games .: "warhammer_fantasy"
  pure warhammerFantasy
parseWarhammerFantasy settings val =
  fail $ "expected Object, but got " <> (show val)

parseWarhammerFantasyField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseWarhammerFantasyField settings txt val = do
  warhammerFantasy <- parseWarhammerFantasy settings val
  field <- warhammerFantasy .:? txt .!= mempty
  pure field

parseWarhammerFantasyFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseWarhammerFantasyFields settings txts val = do
  warhammerFantasy <- parseWarhammerFantasy settings val
  helper warhammerFantasy txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "warhammerFantasy" "heros")

$(genProvider "warhammerFantasy" "heros")

$(genParser "warhammerFantasy" "quotes")

$(genProvider "warhammerFantasy" "quotes")

$(genParser "warhammerFantasy" "locations")

$(genProvider "warhammerFantasy" "locations")

$(genParser "warhammerFantasy" "factions")

$(genProvider "warhammerFantasy" "factions")

$(genParser "warhammerFantasy" "creatures")

$(genProvider "warhammerFantasy" "creatures")
