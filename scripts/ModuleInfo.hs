module ModuleInfo where

data ModuleInfo = ModuleInfo
  { moduleName :: String -- eg: buffy
  , jsonField :: String
  , fields :: [String]
  , unresolvedFields :: [String]
  , nestedFields :: [[String]]
  , unresolvedNestedFields :: [[String]]
  } deriving (Show, Eq, Ord)

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
    , unresolvedNestedFields = []
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
    , unresolvedNestedFields = []
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
    , unresolvedNestedFields = []
    }

compass :: ModuleInfo
compass =
  ModuleInfo
    { moduleName = "compass"
    , jsonField = "compass"
    , fields = []
    , unresolvedFields = ["direction", "abbreviation", "azimuth"]
    , nestedFields =
        [ ["cardinal", "word"]
        , ["cardinal", "abbreviation"]
        , ["cardinal", "azimuth"]
        , ["ordinal", "word"]
        , ["ordinal", "abbreviation"]
        , ["ordinal", "azimuth"]
        , ["half-wind", "word"]
        , ["half-wind", "abbreviation"]
        , ["half-wind", "azimuth"]
        , ["quarter-wind", "word"]
        , ["quarter-wind", "abbreviation"]
        , ["quarter-wind", "azimuth"]
        ]
    , unresolvedNestedFields = []
    }

electricalComponents :: ModuleInfo
electricalComponents =
  ModuleInfo
    { moduleName = "electricalComponents"
    , jsonField = "electrical_components"
    , fields = ["active", "passive", "electromechanical"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

esport :: ModuleInfo
esport =
  ModuleInfo
    { moduleName = "esport"
    , jsonField = "esport"
    , fields = ["players", "teams", "events", "leagues", "games"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

fallout :: ModuleInfo
fallout =
  ModuleInfo
    { moduleName = "fallout"
    , jsonField = "fallout"
    , fields = ["characters", "factions", "locations", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

familyGuy :: ModuleInfo
familyGuy =
  ModuleInfo
    { moduleName = "familyGuy"
    , jsonField = "family_guy"
    , fields = ["character", "location", "quote"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

file :: ModuleInfo
file =
  ModuleInfo
    { moduleName = "file"
    , jsonField = "file"
    , fields = ["extension", "mime_type"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

food :: ModuleInfo
food =
  ModuleInfo
    { moduleName = "food"
    , jsonField = "food"
    , fields =
        [ "dish"
        , "descriptions"
        , "ingredients"
        , "fruits"
        , "vegetables"
        , "spices"
        , "measurements"
        , "measurement_sizes"
        , "metric_measurements"
        , "sushi"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

football :: ModuleInfo
football =
  ModuleInfo
    { moduleName = "football"
    , jsonField = "football"
    , fields = ["teams", "players", "coaches", "competitions", "positions"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

fpba :: ModuleInfo
fpba =
  ModuleInfo
    { moduleName = "freshPrinceOfBelAir"
    , jsonField = "the_fresh_prince_of_bel_air"
    , fields = ["characters", "celebrities", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

friends :: ModuleInfo
friends =
  ModuleInfo
    { moduleName = "friends"
    , jsonField = "friends"
    , fields = ["characters", "locations", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

funnyName :: ModuleInfo
funnyName =
  ModuleInfo
    { moduleName = "funnyName"
    , jsonField = "funny_name"
    , fields = ["name"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

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
    , unresolvedNestedFields = []
    }

elderScrolls :: ModuleInfo
elderScrolls =
  ModuleInfo
    { moduleName = "elderScrolls"
    , jsonField = "elder_scrolls"
    , fields =
        [ "race"
        , "creature"
        , "region"
        , "dragon"
        , "city"
        , "firsrt_name"
        , "last_name"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

currentOne :: ModuleInfo
currentOne = elderScrolls
