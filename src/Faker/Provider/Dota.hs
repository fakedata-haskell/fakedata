{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Dota where

import Config
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


parseDota :: FromJSON a => FakerSettings -> Value -> Parser a
parseDota settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  games <- faker .: "games"
  dota <- games .: "dota"
  pure dota
parseDota settings val = fail $ "expected Object, but got " <> (show val)

parseDotaField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseDotaField settings txt val = do
  dota <- parseDota settings val
  field <- dota .:? txt .!= mempty
  pure field

parseDotaFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseDotaFields settings txts val = do
  dota <- parseDota settings val
  helper dota txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "dota" "hero")

$(genProvider "dota" "hero")

$(genParser "dota" "item")

$(genProvider "dota" "item")

$(genParser "dota" "team")

$(genProvider "dota" "team")

$(genParser "dota" "player")

$(genProvider "dota" "player")

$(genParsers "dota" ["abaddon", "quote"])

$(genProviders "dota" ["abaddon", "quote"])

$(genParsers "dota" ["alchemist", "quote"])

$(genProviders "dota" ["alchemist", "quote"])

$(genParsers "dota" ["axe", "quote"])

$(genProviders "dota" ["axe", "quote"])

$(genParsers "dota" ["beastmaster", "quote"])

$(genProviders "dota" ["beastmaster", "quote"])

$(genParsers "dota" ["brewmaster", "quote"])

$(genProviders "dota" ["brewmaster", "quote"])

$(genParsers "dota" ["bristleback", "quote"])

$(genProviders "dota" ["bristleback", "quote"])

$(genParsers "dota" ["centaur", "quote"])

$(genProviders "dota" ["centaur", "quote"])

$(genParsers "dota" ["chaos_knight", "quote"])

$(genProviders "dota" ["chaos_knight", "quote"])

$(genParsers "dota" ["clockwerk", "quote"])

$(genProviders "dota" ["clockwerk", "quote"])

$(genParsers "dota" ["doom", "quote"])

$(genProviders "dota" ["doom", "quote"])

$(genParsers "dota" ["dragon_knight", "quote"])

$(genProviders "dota" ["dragon_knight", "quote"])

$(genParsers "dota" ["earth_spirit", "quote"])

$(genProviders "dota" ["earth_spirit", "quote"])

$(genParsers "dota" ["earthshaker", "quote"])

$(genProviders "dota" ["earthshaker", "quote"])

$(genParsers "dota" ["elder_titan", "quote"])

$(genProviders "dota" ["elder_titan", "quote"])

$(genParsers "dota" ["huskar", "quote"])

$(genProviders "dota" ["huskar", "quote"])

$(genParsers "dota" ["io", "quote"])

$(genProviders "dota" ["io", "quote"])

$(genParsers "dota" ["kunkka", "quote"])

$(genProviders "dota" ["kunkka", "quote"])

$(genParsers "dota" ["legion_commander", "quote"])

$(genProviders "dota" ["legion_commander", "quote"])

$(genParsers "dota" ["lifestealer", "quote"])

$(genProviders "dota" ["lifestealer", "quote"])

$(genParsers "dota" ["lycan", "quote"])

$(genProviders "dota" ["lycan", "quote"])

$(genParsers "dota" ["magnus", "quote"])

$(genProviders "dota" ["magnus", "quote"])

$(genParsers "dota" ["night_stalker", "quote"])

$(genProviders "dota" ["night_stalker", "quote"])

$(genParsers "dota" ["omniknight", "quote"])

$(genProviders "dota" ["omniknight", "quote"])

$(genParsers "dota" ["phoenix", "quote"])

$(genProviders "dota" ["phoenix", "quote"])

$(genParsers "dota" ["pudge", "quote"])

$(genProviders "dota" ["pudge", "quote"])

$(genParsers "dota" ["sand_king", "quote"])

$(genProviders "dota" ["sand_king", "quote"])

$(genParsers "dota" ["slardar", "quote"])

$(genProviders "dota" ["slardar", "quote"])

$(genParsers "dota" ["spirit_breaker", "quote"])

$(genProviders "dota" ["spirit_breaker", "quote"])

$(genParsers "dota" ["sven", "quote"])

$(genProviders "dota" ["sven", "quote"])

$(genParsers "dota" ["tidehunter", "quote"])

$(genProviders "dota" ["tidehunter", "quote"])

$(genParsers "dota" ["timbersaw", "quote"])

$(genProviders "dota" ["timbersaw", "quote"])

$(genParsers "dota" ["tiny", "quote"])

$(genProviders "dota" ["tiny", "quote"])

$(genParsers "dota" ["tusk", "quote"])

$(genProviders "dota" ["tusk", "quote"])

$(genParsers "dota" ["underlord", "quote"])

$(genProviders "dota" ["underlord", "quote"])

$(genParsers "dota" ["undying", "quote"])

$(genProviders "dota" ["undying", "quote"])

$(genParsers "dota" ["wraith_king", "quote"])

$(genProviders "dota" ["wraith_king", "quote"])

$(genParsers "dota" ["meepo", "quote"])

$(genProviders "dota" ["meepo", "quote"])
