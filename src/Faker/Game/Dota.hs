{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Game.Dota where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Dota
import Faker.TH

$(generateFakeField "dota" "hero")

$(generateFakeField "dota" "item")

$(generateFakeField "dota" "team")

$(generateFakeField "dota" "player")

$(generateFakeFields "dota" ["abaddon", "quote"])

$(generateFakeFields "dota" ["alchemist", "quote"])

$(generateFakeFields "dota" ["axe", "quote"])

$(generateFakeFields "dota" ["beastmaster", "quote"])

$(generateFakeFields "dota" ["brewmaster", "quote"])

$(generateFakeFields "dota" ["bristleback", "quote"])

$(generateFakeFields "dota" ["centaur", "quote"])

$(generateFakeFields "dota" ["chaos_knight", "quote"])

$(generateFakeFields "dota" ["clockwerk", "quote"])

$(generateFakeFields "dota" ["doom", "quote"])

$(generateFakeFields "dota" ["dragon_knight", "quote"])

$(generateFakeFields "dota" ["earth_spirit", "quote"])

$(generateFakeFields "dota" ["earthshaker", "quote"])

$(generateFakeFields "dota" ["elder_titan", "quote"])

$(generateFakeFields "dota" ["huskar", "quote"])

$(generateFakeFields "dota" ["io", "quote"])

$(generateFakeFields "dota" ["kunkka", "quote"])

$(generateFakeFields "dota" ["legion_commander", "quote"])

$(generateFakeFields "dota" ["lifestealer", "quote"])

$(generateFakeFields "dota" ["lycan", "quote"])

$(generateFakeFields "dota" ["magnus", "quote"])

$(generateFakeFields "dota" ["night_stalker", "quote"])

$(generateFakeFields "dota" ["omniknight", "quote"])

$(generateFakeFields "dota" ["phoenix", "quote"])

$(generateFakeFields "dota" ["pudge", "quote"])

$(generateFakeFields "dota" ["sand_king", "quote"])

$(generateFakeFields "dota" ["slardar", "quote"])

$(generateFakeFields "dota" ["spirit_breaker", "quote"])

$(generateFakeFields "dota" ["sven", "quote"])

$(generateFakeFields "dota" ["tidehunter", "quote"])

$(generateFakeFields "dota" ["timbersaw", "quote"])

$(generateFakeFields "dota" ["tiny", "quote"])

$(generateFakeFields "dota" ["tusk", "quote"])

$(generateFakeFields "dota" ["underlord", "quote"])

$(generateFakeFields "dota" ["undying", "quote"])

$(generateFakeFields "dota" ["wraith_king", "quote"])

$(generateFakeFields "dota" ["meepo", "quote"])
