{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.LeagueOfLegends where

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

parseLeagueOfLegends :: FromJSON a => FakerSettings -> Value -> Parser a
parseLeagueOfLegends settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  leagueOfLegends <- faker .: "league_of_legends"
  pure leagueOfLegends
parseLeagueOfLegends settings val = fail $ "expected Object, but got " <> (show val)

parseLeagueOfLegendsField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseLeagueOfLegendsField settings txt val = do
  leagueOfLegends <- parseLeagueOfLegends settings val
  field <- leagueOfLegends .:? txt .!= mempty
  pure field

parseLeagueOfLegendsFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseLeagueOfLegendsFields settings txts val = do
  leagueOfLegends <- parseLeagueOfLegends settings val
  helper leagueOfLegends txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "leagueOfLegends" "champion")

$(genProvider "leagueOfLegends" "champion")


$(genParser "leagueOfLegends" "location")

$(genProvider "leagueOfLegends" "location")


$(genParser "leagueOfLegends" "quote")

$(genProvider "leagueOfLegends" "quote")


$(genParser "leagueOfLegends" "summoner_spell")

$(genProvider "leagueOfLegends" "summoner_spell")


$(genParser "leagueOfLegends" "masteries")

$(genProvider "leagueOfLegends" "masteries")


$(genParser "leagueOfLegends" "rank")

$(genProvider "leagueOfLegends" "rank")











