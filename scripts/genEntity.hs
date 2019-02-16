#!/usr/bin/env stack
-- stack script --resolver lts-12.7
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Char (toUpper)
import Data.String.Interpolate
import System.FilePath ((<.>))

data ModuleInfo = ModuleInfo
  { moduleName :: String -- eg: buffy
  , jsonField :: String
  , fields :: [String]
  , unresolvedFields :: [String]
  , nestedFields :: [[String]]
  } deriving (Show, Eq, Ord)

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = (toUpper x) : xs

moduleDef :: ModuleInfo -> String
moduleDef mod@ModuleInfo {..} =
  let capModname = capitalize moduleName
      fakeField :: String -> String
      fakeField str =
        [i|
$(generateFakeField "#{moduleName}" "#{str}")
|]
      fakeFields :: String
      fakeFields = concat $ map fakeField fields
      ufakeField :: String -> String
      ufakeField str =
        [i|
$(generateFakeFieldUnresolved "#{moduleName}" "#{str}")
|]
      ufakeFields :: String
      ufakeFields = concat $ map ufakeField unresolvedFields
      nfakeField :: [String] -> String
      nfakeField str =
        [i|
$(generateFakeFields "#{moduleName}" #{show str})
|]
      nfakeFields :: String
      nfakeFields = concat $ map nfakeField nestedFields
   in [i|
{-# LANGUAGE TemplateHaskell #-}

module Faker.#{capModname} where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.#{capModname}
import Faker.TH

#{fakeFields}

#{ufakeFields}

#{nfakeFields}

|]

cofee :: ModuleInfo
cofee =
  ModuleInfo
    { moduleName = "coffee"
    , jsonField = "coffee"
    , fields = ["country"]
    , unresolvedFields = []
    , nestedFields =
        [ ["regions", "brazil"]
        , ["regions", "colombia"]
        , ["regions", "sumatra"]
        , ["regions", "ethiopia"]
        , ["regions", "honduras"]
        , ["regions", "kenya"]
        , ["regions", "uganda"]
        , ["regions", "mexico"]
        , ["regions", "guatemala"]
        , ["regions", "nicaragua"]
        , ["regions", "costa_rica"]
        , ["regions", "tanzania"]
        , ["regions", "el_salvador"]
        , ["regions", "rwanda"]
        , ["regions", "burundi"]
        , ["regions", "panama"]
        , ["regions", "yemen"]
        , ["regions", "india"]
        ]
    }

commerce :: ModuleInfo
commerce =
  ModuleInfo
    { moduleName = "commerce"
    , jsonField = "commerce"
    , fields = ["department"]
    , unresolvedFields = []
    , nestedFields =
        [ ["product_name", "adjective"]
        , ["product_name", "material"]
        , ["product_name", "product"]
        , ["promotion_code", "adjective"]
        , ["promotion_code", "noun"]
        ]
    }

dota :: ModuleInfo
dota =
  ModuleInfo
    { moduleName = "dota"
    , jsonField = "dota"
    , fields = ["hero", "iteam", "team", "player"]
    , nestedFields =
        [ ["abaddon", "quote"]
        , ["alchemist", "quote"]
        , ["axe", "quote"]
        , ["beastmaster", "quote"]
        , ["brewmaster", "quote"]
        , ["bristleback", "quote"]
        , ["centaur", "quote"]
        , ["chaos_knight", "quote"]
        , ["clockwerk", "quote"]
        , ["doom", "quote"]
        , ["dragon_knight", "quote"]
        , ["earth_knight", "quote"]
        , ["earthshaker", "quote"]
        , ["elder_titan", "quote"]
        , ["huskar", "quote"]
        , ["io", "quote"]
        , ["kunkka", "quote"]
        , ["legion_commander", "quote"]
        , ["lifestealer", "quote"]
        , ["lycan", "quote"]
        , ["magnus", "quote"]
        , ["night_stalker", "quote"]
        , ["omniknight", "quote"]
        , ["phoenix", "quote"]
        , ["pudge", "quote"]
        , ["sand_king", "quote"]
        , ["slardar", "quote"]
        , ["spirit_breaker", "quote"]
        , ["sven", "quote"]
        , ["tidehunter", "quote"]
        , ["timbersaw", "quote"]
        , ["tiny", "quote"]
        , ["tusk", "quote"]
        , ["underlord", "quote"]
        , ["undying", "quote"]
        , ["wraith_king", "quote"]
        ]
    , unresolvedFields = []
    }

dune :: ModuleInfo
dune =
  ModuleInfo
    { moduleName = "dune"
    , jsonField = "dune"
    , fields = ["characters", "titles", "planets"]
    , unresolvedFields = []
    , nestedFields =
        [ ["quotes", "guild_navigator"]
        , ["quotes", "emperor"]
        , ["quotes", "paul"]
        , ["quotes", "thufir"]
        , ["quotes", "jessica"]
        , ["quotes", "irulan"]
        , ["quotes", "mohiam"]
        , ["quotes", "gurney"]
        , ["quotes", "leto"]
        , ["quotes", "stilgar"]
        , ["quotes", "liet_kynes"]
        , ["quotes", "pardot_kynes"]
        , ["quotes", "baron_harkonnen"]
        , ["quotes", "piter"]
        , ["quotes", "alia"]
        , ["quotes", "mapes"]
        , ["quotes", "duncan"]
        , ["quotes", "yueh"]
        , ["sayings", "bene_gesserit"]
        , ["sayings", "fremen"]
        , ["sayings", "mentat"]
        , ["sayings", "muaddib"]
        , ["sayings", "orange_catholic_bible"]
        ]
    }

generateModule :: ModuleInfo -> IO ()
generateModule m@ModuleInfo {..} = do
  let fname = (capitalize moduleName) <.> "hs"
      dat = moduleDef m
  writeFile fname dat

main :: IO ()
main = generateModule dune
