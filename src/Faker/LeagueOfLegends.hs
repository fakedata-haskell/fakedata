{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.LeagueOfLegends where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.LeagueOfLegends
import Faker.TH

$(generateFakeField "leagueOfLegends" "champion")

$(generateFakeField "leagueOfLegends" "location")

$(generateFakeField "leagueOfLegends" "quote")

$(generateFakeField "leagueOfLegends" "summoner_spell")

$(generateFakeField "leagueOfLegends" "masteries")

$(generateFakeField "leagueOfLegends" "rank")
