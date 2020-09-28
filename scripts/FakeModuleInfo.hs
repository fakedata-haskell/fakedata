module FakeModuleInfo where

data ModuleInfo =
  ModuleInfo
    { mmoduleName :: String -- eg: buffy. Module name. This should be
                            -- the value in mapSource fucntion too.
    , jsonField :: String -- ^ Field after en.faker
    , fields :: [String]
    , unresolvedFields :: [String]
    , nestedFields :: [[String]]
    , unresolvedNestedFields :: [[String]]
    }
  deriving (Show, Eq, Ord)

commerce :: ModuleInfo
commerce =
  ModuleInfo
    { mmoduleName = "commerce"
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
    { mmoduleName = "dune"
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
    { mmoduleName = "dota"
    , jsonField = "dota"
    , fields = ["hero", "item", "team", "player"]
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
        , ["earth_spirit", "quote"]
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
        , ["meepo", "quote"]
        ]
    , unresolvedFields = []
    , unresolvedNestedFields = []
    }

compass :: ModuleInfo
compass =
  ModuleInfo
    { mmoduleName = "compass"
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
    { mmoduleName = "electricalComponents"
    , jsonField = "electrical_components"
    , fields = ["active", "passive", "electromechanical"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

esport :: ModuleInfo
esport =
  ModuleInfo
    { mmoduleName = "esport"
    , jsonField = "esport"
    , fields = ["players", "teams", "events", "leagues", "games"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

fallout :: ModuleInfo
fallout =
  ModuleInfo
    { mmoduleName = "fallout"
    , jsonField = "fallout"
    , fields = ["characters", "factions", "locations", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

familyGuy :: ModuleInfo
familyGuy =
  ModuleInfo
    { mmoduleName = "familyGuy"
    , jsonField = "family_guy"
    , fields = ["character", "location", "quote"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

file :: ModuleInfo
file =
  ModuleInfo
    { mmoduleName = "file"
    , jsonField = "file"
    , fields = ["extension", "mime_type"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

food :: ModuleInfo
food =
  ModuleInfo
    { mmoduleName = "food"
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
    { mmoduleName = "football"
    , jsonField = "football"
    , fields = ["teams", "players", "coaches", "competitions", "positions"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

fpba :: ModuleInfo
fpba =
  ModuleInfo
    { mmoduleName = "freshPrinceOfBelAir"
    , jsonField = "the_fresh_prince_of_bel_air"
    , fields = ["characters", "celebrities", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

friends :: ModuleInfo
friends =
  ModuleInfo
    { mmoduleName = "friends"
    , jsonField = "friends"
    , fields = ["characters", "locations", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

funnyName :: ModuleInfo
funnyName =
  ModuleInfo
    { mmoduleName = "funnyName"
    , jsonField = "funny_name"
    , fields = ["name"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

cofee :: ModuleInfo
cofee =
  ModuleInfo
    { mmoduleName = "coffee"
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
    { mmoduleName = "elderScrolls"
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

got :: ModuleInfo
got =
  ModuleInfo
    { mmoduleName = "gameOfThrones"
    , jsonField = "game_of_thrones"
    , fields = ["characters", "houses", "cities", "quotes", "dragons"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

hacker :: ModuleInfo
hacker =
  ModuleInfo
    { mmoduleName = "hacker"
    , jsonField = "hacker"
    , fields = ["abbreviation", "adjective", "noun", "verb", "ingverb"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

gender :: ModuleInfo
gender =
  ModuleInfo
    { mmoduleName = "gender"
    , jsonField = "gender"
    , fields = ["types", "binary_types"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

gbusters :: ModuleInfo
gbusters =
  ModuleInfo
    { mmoduleName = "ghostbusters"
    , jsonField = "ghostbusters"
    , fields = ["actors", "characters", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

backToTheFuture :: ModuleInfo
backToTheFuture =
  ModuleInfo
    { mmoduleName = "backToTheFuture"
    , jsonField = "back_to_the_future"
    , fields = ["dates", "characters", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

gdead :: ModuleInfo
gdead =
  ModuleInfo
    { mmoduleName = "gratefulDead"
    , jsonField = "grateteful_dead"
    , fields = ["players", "songs"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

gphilosophers :: ModuleInfo
gphilosophers =
  ModuleInfo
    { mmoduleName = "greekPhilosophers"
    , jsonField = "greek_philosophers"
    , fields = ["names", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

halfLife :: ModuleInfo
halfLife =
  ModuleInfo
    { mmoduleName = "halfLife"
    , jsonField = "half_life"
    , fields = ["character", "enemy", "location"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

harryPotter :: ModuleInfo
harryPotter =
  ModuleInfo
    { mmoduleName = "harryPotter"
    , jsonField = "harry_potter"
    , fields =
        ["characters", "locations", "quotes", "books", "houses", "spells"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

heroes :: ModuleInfo
heroes =
  ModuleInfo
    { mmoduleName = "heroes"
    , jsonField = "heroes"
    , fields = ["names", "specialties", "klasses"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

hofs :: ModuleInfo
hofs =
  ModuleInfo
    { mmoduleName = "heroesOfTheStorm"
    , jsonField = "heroes_of_the_storm"
    , fields = ["battlegrounds", "classes", "heroes", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

heyArnold :: ModuleInfo
heyArnold =
  ModuleInfo
    { mmoduleName = "heyArnold"
    , jsonField = "hey_arnold"
    , fields = ["characters", "locations", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

hgtg :: ModuleInfo
hgtg =
  ModuleInfo
    { mmoduleName = "hitchhikersGuideToTheGalaxy"
    , jsonField = "hitchhikers_guide_to_the_galaxy"
    , fields =
        [ "characters"
        , "locations"
        , "marvin_quote"
        , "planets"
        , "quotes"
        , "species"
        , "starships"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

house :: ModuleInfo
house =
  ModuleInfo
    { mmoduleName = "house"
    , jsonField = "house"
    , fields = ["furniture", "rooms"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

hwmym :: ModuleInfo
hwmym =
  ModuleInfo
    { mmoduleName = "howIMetYourMother"
    , jsonField = "how_i_met_your_mother"
    , fields = ["character", "catch_phrase", "high_five", "quote"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

isegments :: ModuleInfo
isegments =
  ModuleInfo
    { mmoduleName = "industrySegments"
    , jsonField = "industry_segments"
    , fields = ["industry", "super_sector", "sector", "sub_sector"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

internet :: ModuleInfo
internet =
  ModuleInfo
    { mmoduleName = "internet"
    , jsonField = "internet"
    , fields = ["free_email", "domain_suffix"]
    , unresolvedFields = []
    , nestedFields =
        [ ["user_agent", "aol"]
        , ["user_agent", "chrome"]
        , ["user_agent", "firefox"]
        , ["user_agent", "internet_explorer"]
        , ["user_agent", "netscape"]
        , ["user_agent", "opera"]
        , ["user_agent", "safari"]
        ]
    , unresolvedNestedFields = []
    }

job :: ModuleInfo
job =
  ModuleInfo
    { mmoduleName = "job"
    , jsonField = "job"
    , fields =
        [ "field"
        , "seniority"
        , "position"
        , "key_skills"
        , "employment_type"
        , "education_level"
        ]
    , unresolvedFields = ["title"]
    , nestedFields = []
    , unresolvedNestedFields = []
    }

kpop :: ModuleInfo
kpop =
  ModuleInfo
    { mmoduleName = "kpop"
    , jsonField = "kpop"
    , fields =
        [ "i_groups"
        , "ii_groups"
        , "iii_groups"
        , "girl_groups"
        , "boy_bands"
        , "solo"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

lol :: ModuleInfo
lol =
  ModuleInfo
    { mmoduleName = "leagueOfLegends"
    , jsonField = "league_of_legends"
    , fields =
        ["champion", "location", "quote", "summoner_spell", "masteries", "rank"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

lebowski :: ModuleInfo
lebowski =
  ModuleInfo
    { mmoduleName = "lebowski"
    , jsonField = "lebowski"
    , fields = ["actors", "characters", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

lofr :: ModuleInfo
lofr =
  ModuleInfo
    { mmoduleName = "lordOfTheRings"
    , jsonField = "lord_of_the_rings"
    , fields = ["characters", "locations", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

lorem :: ModuleInfo
lorem =
  ModuleInfo
    { mmoduleName = "lorem"
    , jsonField = "lorem"
    , fields = ["words", "supplemental"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

lovecraft :: ModuleInfo
lovecraft =
  ModuleInfo
    { mmoduleName = "lovecraft"
    , jsonField = "lovecraft"
    , fields = ["fhtagn", "deity", "location", "tome", "words"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

markdown :: ModuleInfo
markdown =
  ModuleInfo
    { mmoduleName = "markdown"
    , jsonField = "markdown"
    , fields = ["headers", "emphasis"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

marketing :: ModuleInfo
marketing =
  ModuleInfo
    { mmoduleName = "marketing"
    , jsonField = "marketing"
    , fields = ["buzzwords"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

measurement :: ModuleInfo
measurement =
  ModuleInfo
    { mmoduleName = "measurement"
    , jsonField = "measurement"
    , fields =
        [ "height"
        , "length"
        , "volume"
        , "weight"
        , "metric_height"
        , "metric_length"
        , "metric_volume"
        , "metric_weight"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

michaelScott :: ModuleInfo
michaelScott =
  ModuleInfo
    { mmoduleName = "michaelScott"
    , jsonField = "michael_scott"
    , fields = ["quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

military :: ModuleInfo
military =
  ModuleInfo
    { mmoduleName = "military"
    , jsonField = "military"
    , fields =
        [ "army_rank"
        , "marines_rank"
        , "navy_rank"
        , "air_force_rank"
        , "dod_paygrade"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

movie :: ModuleInfo
movie =
  ModuleInfo
    { mmoduleName = "movie"
    , jsonField = "movie"
    , fields = ["quote"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

music :: ModuleInfo
music =
  ModuleInfo
    { mmoduleName = "music"
    , jsonField = "music"
    , fields = ["instruments", "bands", "albums", "genres"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

myst :: ModuleInfo
myst =
  ModuleInfo
    { mmoduleName = "myst"
    , jsonField = "myst"
    , fields = ["games", "creatures", "characters", "ages", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

nation :: ModuleInfo
nation =
  ModuleInfo
    { mmoduleName = "nation"
    , jsonField = "nation"
    , fields = ["nationality", "language", "capital_city"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

npa :: ModuleInfo
npa =
  ModuleInfo
    { mmoduleName = "natoPhoneticAlphabet"
    , jsonField = "nato_phonetic_alphabet"
    , fields = ["code_word"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

newGirl :: ModuleInfo
newGirl =
  ModuleInfo
    { mmoduleName = "newGirl"
    , jsonField = "new_girl"
    , fields = ["characters", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

onePiece :: ModuleInfo
onePiece =
  ModuleInfo
    { mmoduleName = "onePiece"
    , jsonField = "one_piece"
    , fields =
        ["characters", "seas", "islands", "locations", "quotes", "akumas_no_mi"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

parksAndRec :: ModuleInfo
parksAndRec =
  ModuleInfo
    { mmoduleName = "parksAndRec"
    , jsonField = "parks_and_rec"
    , fields = ["characters", "cities"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

phish :: ModuleInfo
phish =
  ModuleInfo
    { mmoduleName = "phish"
    , jsonField = "phish"
    , fields = ["song"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

pokemon :: ModuleInfo
pokemon =
  ModuleInfo
    { mmoduleName = "pokemon"
    , jsonField = "pokemon"
    , fields = ["names", "locations", "moves"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

pbride :: ModuleInfo
pbride =
  ModuleInfo
    { mmoduleName = "princessBride"
    , jsonField = "princess_bride"
    , fields = ["characters", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

pl :: ModuleInfo
pl =
  ModuleInfo
    { mmoduleName = "programmingLauguage"
    , jsonField = "programming_language"
    , fields = ["name", "creator"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

quote :: ModuleInfo
quote =
  ModuleInfo
    { mmoduleName = "quote"
    , jsonField = "quote"
    , fields =
        [ "famous_last_words"
        , "matz"
        , "most_interesting_man_in_the_world"
        , "robin"
        , "singular_siegler"
        , "yoda"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

relationship :: ModuleInfo
relationship =
  ModuleInfo
    { mmoduleName = "relationship"
    , jsonField = "relationship"
    , fields = ["in_law", "spouse", "parent", "sibling"]
    , unresolvedFields = []
    , nestedFields = [["familial", "direct"], ["familial", "extended"]]
    , unresolvedNestedFields = []
    }

ram :: ModuleInfo
ram =
  ModuleInfo
    { mmoduleName = "rickAndMorty"
    , jsonField = "rick_and_morty"
    , fields = ["characters", "locations", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

rockBand :: ModuleInfo
rockBand =
  ModuleInfo
    { mmoduleName = "rockBand"
    , jsonField = "rock_band"
    , fields = ["name"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

rupaul :: ModuleInfo
rupaul =
  ModuleInfo
    { mmoduleName = "rupaul"
    , jsonField = "rupaul"
    , fields = ["queens", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

science :: ModuleInfo
science =
  ModuleInfo
    { mmoduleName = "science"
    , jsonField = "science"
    , fields = ["element", "element_symbol", "scientist"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

seinfeld :: ModuleInfo
seinfeld =
  ModuleInfo
    { mmoduleName = "seinfeld"
    , jsonField = "seinfeld"
    , fields = ["character", "quote", "business"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

shakespeare :: ModuleInfo
shakespeare =
  ModuleInfo
    { mmoduleName = "shakespeare"
    , jsonField = "shakespeare"
    , fields =
        ["hamlet", "as_you_like_it", "king_richard_iii", "romeo_and_juliet"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

siliconValley :: ModuleInfo
siliconValley =
  ModuleInfo
    { mmoduleName = "siliconValley"
    , jsonField = "silicon_valley"
    , fields =
        [ "characters"
        , "companies"
        , "quotes"
        , "apps"
        , "inventions"
        , "mottos"
        , "urls"
        , "email"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

simpsons :: ModuleInfo
simpsons =
  ModuleInfo
    { mmoduleName = "simpsons"
    , jsonField = "simpsons"
    , fields = ["characters", "locations", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

slackEmoji :: ModuleInfo
slackEmoji =
  ModuleInfo
    { mmoduleName = "slackEmoji"
    , jsonField = "slack_emoji"
    , fields =
        [ "people"
        , "nature"
        , "food_and_drink"
        , "celebration"
        , "activity"
        , "travel_and_places"
        , "objects_and_symbols"
        , "custom"
        ]
    , unresolvedFields = ["emoji"]
    , nestedFields = []
    , unresolvedNestedFields = []
    }

snh :: ModuleInfo
snh =
  ModuleInfo
    { mmoduleName = "sonicTheHedgehog"
    , jsonField = "sonic_the_hedgehog"
    , fields = ["zone", "character", "game"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

source :: ModuleInfo
source =
  ModuleInfo
    { mmoduleName = "source"
    , jsonField = "source"
    , fields = []
    , unresolvedFields = []
    , nestedFields =
        [ ["hello_world", "ruby"]
        , ["hello_world", "javascript"]
        , ["print", "ruby"]
        , ["print", "javascript"]
        , ["print_1_to_10", "ruby"]
        , ["print_1_to_10", "javascript"]
        ]
    , unresolvedNestedFields = []
    }

southPark :: ModuleInfo
southPark =
  ModuleInfo
    { mmoduleName = "southPark"
    , jsonField = "south_park"
    , fields = ["characters", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

space :: ModuleInfo
space =
  ModuleInfo
    { mmoduleName = "space"
    , jsonField = "space"
    , fields =
        [ "planet"
        , "moon"
        , "galaxy"
        , "nebula"
        , "star_cluster"
        , "constellation"
        , "star"
        , "agency"
        , "agency_abv"
        , "nasa_space_craft"
        , "company"
        , "distance_measurement"
        , "meteorite"
        , "launch_vehicle"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

strek :: ModuleInfo
strek =
  ModuleInfo
    { mmoduleName = "starTrek"
    , jsonField = "star_trek"
    , fields = ["character", "location", "specie", "villain"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

swars :: ModuleInfo
swars =
  ModuleInfo
    { mmoduleName = "starWars"
    , jsonField = "star_wars"
    , fields =
        [ "characters"
        , "call_squadrons"
        , "droids"
        , "planets"
        , "species"
        , "vehicles"
        , "wookiee_words"
        ]
    , unresolvedFields = ["call_numbers", "call_sign"]
    , nestedFields =
        [ ["quotes", "admiral_ackbar"]
        , ["quotes", "ahsoka_tano"]
        , ["quotes", "anakin_skywalker"]
        , ["quotes", "asajj_ventress"]
        , ["quotes", "bendu"]
        , ["quotes", "boba_fett"]
        , ["quotes", "c_3po"]
        , ["quotes", "count_dooku"]
        , ["quotes", "darth_caedus"]
        , ["quotes", "darth_vader"]
        , ["quotes", "emperor_palpatine"]
        , ["quotes", "finn"]
        , ["quotes", "grand_admiral_thrawn"]
        , ["quotes", "grand_moff_tarkin"]
        , ["quotes", "greedo"]
        , ["quotes", "han_solo"]
        , ["quotes", "jabba_the_hutt"]
        , ["quotes", "jar_jar_binks"]
        , ["quotes", "k_2so"]
        , ["quotes", "kylo_ren"]
        , ["quotes", "lando_calrissian"]
        , ["quotes", "leia_organa"]
        , ["quotes", "luke_skywalker"]
        , ["quotes", "mace_windu"]
        , ["quotes", "maz_kanata"]
        , ["quotes", "obi_wan_kenobi"]
        , ["quotes", "padme_amidala"]
        , ["quotes", "qui_gon_jinn"]
        , ["quotes", "rey"]
        , ["quotes", "shmi_skywalker"]
        , ["quotes", "yoda"]
        , ["alternate_character_spellings", "admiral_ackbar"]
        , ["alternate_character_spellings", "ahsoka_tano"]
        , ["alternate_character_spellings", "anakin_skywalker"]
        , ["alternate_character_spellings", "asajj_ventress"]
        , ["alternate_character_spellings", "bendu"]
        , ["alternate_character_spellings", "boba_fett"]
        , ["alternate_character_spellings", "c_3po"]
        , ["alternate_character_spellings", "count_dooku"]
        , ["alternate_character_spellings", "darth_caedus"]
        , ["alternate_character_spellings", "darth_vader"]
        , ["alternate_character_spellings", "emperor_palpatine"]
        , ["alternate_character_spellings", "finn"]
        , ["alternate_character_spellings", "general_hux"]
        , ["alternate_character_spellings", "grand_admiral_thrawn"]
        , ["alternate_character_spellings", "grand_moff_tarkin"]
        , ["alternate_character_spellings", "greedo"]
        , ["alternate_character_spellings", "han_solo"]
        , ["alternate_character_spellings", "jabba_the_hutt"]
        , ["alternate_character_spellings", "jar_jar_binks"]
        , ["alternate_character_spellings", "k_2so"]
        , ["alternate_character_spellings", "kylo_ren"]
        , ["alternate_character_spellings", "lando_calrissian"]
        , ["alternate_character_spellings", "leia_organa"]
        , ["alternate_character_spellings", "luke_skywalker"]
        , ["alternate_character_spellings", "mace_windu"]
        , ["alternate_character_spellings", "maz_kanata"]
        , ["alternate_character_spellings", "obi_wan_kenobi"]
        , ["alternate_character_spellings", "padme_amidala"]
        , ["alternate_character_spellings", "qui_gon_jinn"]
        , ["alternate_character_spellings", "rey"]
        , ["alternate_character_spellings", "shmi_skywalker"]
        , ["alternate_character_spellings", "yoda"]
        ]
    , unresolvedNestedFields = []
    }

stargate :: ModuleInfo
stargate =
  ModuleInfo
    { mmoduleName = "stargate"
    , jsonField = "stargate"
    , fields = ["characters", "planets", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

strangerThings :: ModuleInfo
strangerThings =
  ModuleInfo
    { mmoduleName = "strangerThings"
    , jsonField = "stranger_things"
    , fields = ["character", "quote"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

subscription :: ModuleInfo
subscription =
  ModuleInfo
    { mmoduleName = "subscription"
    , jsonField = "subscription"
    , fields =
        [ "plans"
        , "statuses"
        , "payment_methods"
        , "subscription_terms"
        , "payment_terms"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

ssb :: ModuleInfo
ssb =
  ModuleInfo
    { mmoduleName = "superSmashBros"
    , jsonField = "super_smash_bros"
    , fields = ["fighter", "stage"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

superhero :: ModuleInfo
superhero =
  ModuleInfo
    { mmoduleName = "superhero"
    , jsonField = "superhero"
    , fields = ["power", "prefix", "suffix", "descriptor"]
    , unresolvedFields = ["name"]
    , nestedFields = []
    , unresolvedNestedFields = []
    }

sao :: ModuleInfo
sao =
  ModuleInfo
    { mmoduleName = "swordArtOnline"
    , jsonField = "sword_art_online"
    , fields = ["real_name", "game_name", "location", "item"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

team :: ModuleInfo
team =
  ModuleInfo
    { mmoduleName = "team"
    , jsonField = "team"
    , fields = ["creature", "sport", "mascot"]
    , unresolvedFields = ["name"]
    , nestedFields = []
    , unresolvedNestedFields = []
    }

theExpanse :: ModuleInfo
theExpanse =
  ModuleInfo
    { mmoduleName = "theExpanse"
    , jsonField = "the_expanse"
    , fields = ["characters", "locations", "ships", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

tic :: ModuleInfo
tic =
  ModuleInfo
    { mmoduleName = "theItCrowd"
    , jsonField = "the_it_crowd"
    , fields = ["actors", "characters", "emails", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

ttoi :: ModuleInfo
ttoi =
  ModuleInfo
    { mmoduleName = "theThickOfIt"
    , jsonField = "the_thick_of_it"
    , fields = ["characters", "positions", "departments"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

twinPeaks :: ModuleInfo
twinPeaks =
  ModuleInfo
    { mmoduleName = "twinPeaks"
    , jsonField = "twin_peaks"
    , fields = ["characters", "locations", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

umphreysMcgee :: ModuleInfo
umphreysMcgee =
  ModuleInfo
    { mmoduleName = "umphreysMcgee"
    , jsonField = "umphreys_mcgee"
    , fields = ["song"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

university :: ModuleInfo
university =
  ModuleInfo
    { mmoduleName = "university"
    , jsonField = "university"
    , fields = ["prefix", "suffix"]
    , unresolvedFields = ["name"]
    , nestedFields = []
    , unresolvedNestedFields = []
    }

vfv :: ModuleInfo
vfv =
  ModuleInfo
    { mmoduleName = "vForVendetta"
    , jsonField = "v_for_vendetta"
    , fields = ["characters", "speeches", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

ventureBros :: ModuleInfo
ventureBros =
  ModuleInfo
    { mmoduleName = "ventureBros"
    , jsonField = "venture_bros"
    , fields = ["character", "organization", "vehicle", "quote"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

verbs :: ModuleInfo
verbs =
  ModuleInfo
    { mmoduleName = "verbs"
    , jsonField = "verbs"
    , fields = ["base", "past", "past_participle", "simple_present", "ing_form"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

witcher :: ModuleInfo
witcher =
  ModuleInfo
    { mmoduleName = "witcher"
    , jsonField = "witcher"
    , fields =
        ["characters", "witchers", "schools", "locations", "quotes", "monsters"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

wow :: ModuleInfo
wow =
  ModuleInfo
    { mmoduleName = "worldOfWarcraft"
    , jsonField = "world_of_warcraft"
    , fields = ["hero", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

yoda :: ModuleInfo
yoda =
  ModuleInfo
    { mmoduleName = "yoda"
    , jsonField = "yoda"
    , fields = ["quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

zelda :: ModuleInfo
zelda =
  ModuleInfo
    { mmoduleName = "zelda"
    , jsonField = "zelda"
    , fields = ["games", "characters", "locations", "items"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

phoneNumber :: ModuleInfo
phoneNumber =
  ModuleInfo
    { mmoduleName = "phoneNumber"
    , jsonField = "phone_number"
    , fields = []
    , unresolvedFields = ["formats"]
    , nestedFields = []
    , unresolvedNestedFields = []
    }

wcup :: ModuleInfo
wcup =
  ModuleInfo
    { mmoduleName = "worldCup"
    , jsonField = "world_cup"
    , fields = ["teams", "stadiums", "cities"]
    , unresolvedFields = []
    , nestedFields =
        [ ["groups", "group_A"]
        , ["groups", "group_B"]
        , ["groups", "group_C"]
        , ["groups", "group_D"]
        , ["groups", "group_E"]
        , ["groups", "group_F"]
        , ["groups", "group_G"]
        , ["groups", "group_H"]
        , ["rosters", "Egypt", "coach"]
        , ["rosters", "Egypt", "goalkeepers"]
        , ["rosters", "Egypt", "defenders"]
        , ["rosters", "Egypt", "midfielders"]
        , ["rosters", "Egypt", "forwards"]
        , ["rosters", "Russia", "coach"]
        , ["rosters", "Russia", "goalkeepers"]
        , ["rosters", "Russia", "defenders"]
        , ["rosters", "Russia", "midfielders"]
        , ["rosters", "Russia", "forwards"]
        , ["rosters", "Saudi_Arabia", "coach"]
        , ["rosters", "Saudi_Arabia", "goalkeepers"]
        , ["rosters", "Saudi_Arabia", "defenders"]
        , ["rosters", "Saudi_Arabia", "midfielders"]
        , ["rosters", "Saudi_Arabia", "forwards"]
        , ["rosters", "Uruguay", "coach"]
        , ["rosters", "Uruguay", "goalkeepers"]
        , ["rosters", "Uruguay", "defenders"]
        , ["rosters", "Uruguay", "midfielders"]
        , ["rosters", "Uruguay", "forwards"]
        , ["rosters", "Iran", "coach"]
        , ["rosters", "Iran", "goalkeepers"]
        , ["rosters", "Iran", "defenders"]
        , ["rosters", "Iran", "midfielders"]
        , ["rosters", "Iran", "forwards"]
        , ["rosters", "Morocco", "coach"]
        , ["rosters", "Morocco", "goalkeepers"]
        , ["rosters", "Morocco", "defenders"]
        , ["rosters", "Morocco", "midfielders"]
        , ["rosters", "Morocco", "forwards"]
        , ["rosters", "Portugal", "coach"]
        , ["rosters", "Portugal", "goalkeepers"]
        , ["rosters", "Portugal", "defenders"]
        , ["rosters", "Portugal", "midfielders"]
        , ["rosters", "Portugal", "forwards"]
        , ["rosters", "Spain", "coach"]
        , ["rosters", "Spain", "goalkeepers"]
        , ["rosters", "Spain", "defenders"]
        , ["rosters", "Spain", "midfielders"]
        , ["rosters", "Spain", "forwards"]
        , ["rosters", "Australia", "coach"]
        , ["rosters", "Australia", "goalkeepers"]
        , ["rosters", "Australia", "defenders"]
        , ["rosters", "Australia", "midfielders"]
        , ["rosters", "Australia", "forwards"]
        , ["rosters", "Denmark", "coach"]
        , ["rosters", "Denmark", "goalkeepers"]
        , ["rosters", "Denmark", "defenders"]
        , ["rosters", "Denmark", "midfielders"]
        , ["rosters", "Denmark", "forwards"]
        , ["rosters", "France", "coach"]
        , ["rosters", "France", "goalkeepers"]
        , ["rosters", "France", "defenders"]
        , ["rosters", "France", "midfielders"]
        , ["rosters", "France", "forwards"]
        , ["rosters", "Peru", "coach"]
        , ["rosters", "Peru", "goalkeepers"]
        , ["rosters", "Peru", "defenders"]
        , ["rosters", "Peru", "midfielders"]
        , ["rosters", "Peru", "forwards"]
        , ["rosters", "Argentina", "coach"]
        , ["rosters", "Argentina", "goalkeepers"]
        , ["rosters", "Argentina", "defenders"]
        , ["rosters", "Argentina", "midfielders"]
        , ["rosters", "Argentina", "forwards"]
        , ["rosters", "Croatia", "coach"]
        , ["rosters", "Croatia", "goalkeepers"]
        , ["rosters", "Croatia", "defenders"]
        , ["rosters", "Croatia", "midfielders"]
        , ["rosters", "Croatia", "forwards"]
        , ["rosters", "Iceland", "coach"]
        , ["rosters", "Iceland", "goalkeepers"]
        , ["rosters", "Iceland", "defenders"]
        , ["rosters", "Iceland", "midfielders"]
        , ["rosters", "Iceland", "forwards"]
        , ["rosters", "Nigeria", "coach"]
        , ["rosters", "Nigeria", "goalkeepers"]
        , ["rosters", "Nigeria", "defenders"]
        , ["rosters", "Nigeria", "midfielders"]
        , ["rosters", "Nigeria", "forwards"]
        , ["rosters", "Brazil", "coach"]
        , ["rosters", "Brazil", "goalkeepers"]
        , ["rosters", "Brazil", "defenders"]
        , ["rosters", "Brazil", "midfielders"]
        , ["rosters", "Brazil", "forwards"]
        , ["rosters", "Costa_Rica", "coach"]
        , ["rosters", "Costa_Rica", "goalkeepers"]
        , ["rosters", "Costa_Rica", "defenders"]
        , ["rosters", "Costa_Rica", "midfielders"]
        , ["rosters", "Costa_Rica", "forwards"]
        , ["rosters", "Serbia", "coach"]
        , ["rosters", "Serbia", "goalkeepers"]
        , ["rosters", "Serbia", "defenders"]
        , ["rosters", "Serbia", "midfielders"]
        , ["rosters", "Serbia", "forwards"]
        , ["rosters", "Switzerland", "coach"]
        , ["rosters", "Switzerland", "goalkeepers"]
        , ["rosters", "Switzerland", "defenders"]
        , ["rosters", "Switzerland", "midfielders"]
        , ["rosters", "Switzerland", "forwards"]
        , ["rosters", "Germany", "coach"]
        , ["rosters", "Germany", "goalkeepers"]
        , ["rosters", "Germany", "defenders"]
        , ["rosters", "Germany", "midfielders"]
        , ["rosters", "Germany", "forwards"]
        , ["rosters", "Mexico", "coach"]
        , ["rosters", "Mexico", "goalkeepers"]
        , ["rosters", "Mexico", "defenders"]
        , ["rosters", "Mexico", "midfielders"]
        , ["rosters", "Mexico", "forwards"]
        , ["rosters", "South Korea", "coach"]
        , ["rosters", "South Korea", "goalkeepers"]
        , ["rosters", "South Korea", "defenders"]
        , ["rosters", "South Korea", "midfielders"]
        , ["rosters", "South Korea", "forwards"]
        , ["rosters", "Sweden", "coach"]
        , ["rosters", "Sweden", "goalkeepers"]
        , ["rosters", "Sweden", "defenders"]
        , ["rosters", "Sweden", "midfielders"]
        , ["rosters", "Sweden", "forwards"]
        , ["rosters", "Belgium", "coach"]
        , ["rosters", "Belgium", "goalkeepers"]
        , ["rosters", "Belgium", "defenders"]
        , ["rosters", "Belgium", "midfielders"]
        , ["rosters", "Belgium", "forwards"]
        , ["rosters", "England", "coach"]
        , ["rosters", "England", "goalkeepers"]
        , ["rosters", "England", "defenders"]
        , ["rosters", "England", "midfielders"]
        , ["rosters", "England", "forwards"]
        , ["rosters", "Panama", "coach"]
        , ["rosters", "Panama", "goalkeepers"]
        , ["rosters", "Panama", "defenders"]
        , ["rosters", "Panama", "midfielders"]
        , ["rosters", "Panama", "forwards"]
        , ["rosters", "Tunisia", "coach"]
        , ["rosters", "Tunisia", "goalkeepers"]
        , ["rosters", "Tunisia", "defenders"]
        , ["rosters", "Tunisia", "midfielders"]
        , ["rosters", "Tunisia", "forwards"]
        , ["rosters", "Columbia", "coach"]
        , ["rosters", "Columbia", "goalkeepers"]
        , ["rosters", "Columbia", "defenders"]
        , ["rosters", "Columbia", "midfielders"]
        , ["rosters", "Columbia", "forwards"]
        , ["rosters", "Japan", "coach"]
        , ["rosters", "Japan", "goalkeepers"]
        , ["rosters", "Japan", "defenders"]
        , ["rosters", "Japan", "midfielders"]
        , ["rosters", "Japan", "forwards"]
        , ["rosters", "Poland", "coach"]
        , ["rosters", "Poland", "goalkeepers"]
        , ["rosters", "Poland", "defenders"]
        , ["rosters", "Poland", "midfielders"]
        , ["rosters", "Poland", "forwards"]
        , ["rosters", "Senegal", "coach"]
        , ["rosters", "Senegal", "goalkeepers"]
        , ["rosters", "Senegal", "defenders"]
        , ["rosters", "Senegal", "midfielders"]
        , ["rosters", "Senegal", "forwards"]
        ]
    , unresolvedNestedFields = []
    }

restaurant :: ModuleInfo
restaurant =
  ModuleInfo
    { mmoduleName = "restaurant"
    , jsonField = "restaurant"
    , fields = ["name_suffix", "type", "description", "review"]
    , unresolvedFields = ["name_prefix", "name"]
    , nestedFields = []
    , unresolvedNestedFields = []
    }

vehicle :: ModuleInfo
vehicle =
  ModuleInfo
    { mmoduleName = "vehicle"
    , jsonField = "vehicle"
    , fields =
        [ "manufacture"
        , "makes"
        , "colors"
        , "transmissions"
        , "drive_types"
        , "fuel_types"
        , "styles"
        , "car_types"
        , "car_options"
        , "standard_specs"
        , "doors"
        , "engine_sizes"
        ]
    , unresolvedFields = ["license_plate"]
    , nestedFields =
        [ ["models_by_make", "BMW"]
        , ["models_by_make", "Audi"]
        , ["models_by_make", "Toyota"]
        , ["models_by_make", "Chevy"]
        , ["models_by_make", "Ford"]
        , ["models_by_make", "Dodge"]
        , ["models_by_make", "Lincoln"]
        , ["models_by_make", "Buick"]
        , ["models_by_make", "Honda"]
        , ["models_by_make", "Nissan"]
        ]
    , unresolvedNestedFields = []
    }

basketball :: ModuleInfo
basketball =
  ModuleInfo
    { mmoduleName = "basketball"
    , jsonField = "basketball"
    , fields = ["teams", "players", "coaches", "positions"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

horse :: ModuleInfo
horse =
  ModuleInfo
    { mmoduleName = "horse"
    , jsonField = "horse"
    , fields = ["name", "breed"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

finance :: ModuleInfo
finance =
  ModuleInfo
    { mmoduleName = "finance"
    , jsonField = "finance"
    , fields = []
    , unresolvedFields =
        [ "visa"
        , "mastercard"
        , "discover"
        , "american_express"
        , "diners_club"
        , "jcb"
        , "switch"
        , "solo"
        , "dankort"
        , "maestro"
        , "forbrugsforeningen"
        , "laser"
        ]
    , nestedFields = []
    , unresolvedNestedFields = []
    }

opera :: ModuleInfo
opera =
  ModuleInfo
    { mmoduleName = "opera"
    , jsonField = "opera"
    , fields = []
    , unresolvedFields = []
    , nestedFields =
        [ ["italian", "by_giuseppe_verdi"]
        , ["italian", "by_gioacchino_rossini"]
        , ["italian", "by_gaetano_donizetti"]
        , ["italian", "by_vincenzo_bellini"]
        ]
    , unresolvedNestedFields = []
    }

ancient :: ModuleInfo
ancient =
  ModuleInfo
    { mmoduleName = "ancient"
    , jsonField = "ancient"
    , fields = ["god", "primordial", "hero", "titan"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

appliance :: ModuleInfo
appliance =
  ModuleInfo
    { mmoduleName = "appliance"
    , jsonField = "appliance"
    , fields = ["brand", "equipment"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

artist :: ModuleInfo
artist =
  ModuleInfo
    { mmoduleName = "artist"
    , jsonField = "artist"
    , fields = ["names"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

bank :: ModuleInfo
bank =
  ModuleInfo
    { mmoduleName = "bank"
    , jsonField = "bank"
    , fields = ["name", "swift_bic"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

beer :: ModuleInfo
beer =
  ModuleInfo
    { mmoduleName = "beer"
    , jsonField = "beer"
    , fields = ["name", "brand", "hop", "yeast", "malt", "style"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

name :: ModuleInfo
name =
  ModuleInfo
    { mmoduleName = "name"
    , jsonField = "name"
    , fields =
        [ "male_first_name"
        , "female_first_name"
        , "prefix"
        , "suffix"
        , "last_name"
        ]
    , unresolvedFields = ["name", "name_with_middle", "first_name"]
    , nestedFields = []
    , unresolvedNestedFields = []
    }

blood :: ModuleInfo
blood = ModuleInfo 
    { mmoduleName = "blood"
    , jsonField = "blood"
    , fields =
        [ "type"
        , "rh_factor"
        ]
    , unresolvedFields = ["group"]
    , nestedFields = []
    , unresolvedNestedFields = []
    }

chiquito :: ModuleInfo
chiquito = ModuleInfo
    { mmoduleName = "chiquito"
    , jsonField = "chiquito"
    , fields =
        [ "expressions"
        , "terms"
        , "sentences"
        , "jokes"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

computer :: ModuleInfo
computer = ModuleInfo
    { mmoduleName = "computer"
    , jsonField = "computer"
    , fields =
        [ "type"
        , "platform"
        ]
    , unresolvedFields = []
    , nestedFields = [["os","linux"], ["os","macos"],["os","windows"]]
    , unresolvedNestedFields = []
    }

control :: ModuleInfo
control =
  ModuleInfo
    { mmoduleName = "control"
    , jsonField = "control"
    , fields =
        [ "character"
        , "location"
        , "object_of_power"
        , "altered_item"
        , "altered_world_event"
        , "hiss"
        , "the_board"
        , "quote"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

departed :: ModuleInfo
departed =
  ModuleInfo
    { mmoduleName = "departed"
    , jsonField = "departed"
    , fields =
        [ "actors"
        , "characters"
        , "quotes"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

dnd :: ModuleInfo
dnd =
  ModuleInfo
    { mmoduleName = "dnd"
    , jsonField = "dnd"
    , fields =
        [ 
          "klasses"
        , "alignments"
        , "cities"
        , "languages"
        , "melee_weapons"
        , "monsters"
        , "races"
        , "ranged_weapons"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

pearlJam :: ModuleInfo
pearlJam =
  ModuleInfo
    { mmoduleName = "pearlJam"
    , jsonField = "pearl_jam"
    , fields =
        [ "musicians"
        , "albums"
        , "songs"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

rajnikanth :: ModuleInfo
rajnikanth =
  ModuleInfo
    { mmoduleName = "rajnikanth"
    , jsonField = "rajnikanth"
    , fields =
        [ "joke"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

showM :: ModuleInfo
showM =
  ModuleInfo
    { mmoduleName = "show"
    , jsonField = "show"
    , fields =
        [ "adult_musical"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

suits :: ModuleInfo
suits =
  ModuleInfo
    { mmoduleName = "suits"
    , jsonField = "suits"
    , fields =
        [ "characters", "quotes"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

warhammerFantasy :: ModuleInfo
warhammerFantasy  =
  ModuleInfo
    { mmoduleName = "warhammerFantasy"
    , jsonField = "warhammer_fantasy"
    , fields =
        [ "heros", "quotes", "locations", "factions", "creatures"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

bigBangTheory :: ModuleInfo
bigBangTheory = 
  ModuleInfo
    { mmoduleName = "bigBangTheory"
    , jsonField = "big_bang_theory"
    , fields =
        [ "characters", "quotes"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

futurama :: ModuleInfo
futurama = 
  ModuleInfo
    { mmoduleName = "futurama"
    , jsonField = "futurama"
    , fields =
        [ "characters", "locations", "quotes", "hermes_catchphrases"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

drivingLicense :: ModuleInfo
drivingLicense =
    ModuleInfo
    { mmoduleName = "drivingLicense"
    , jsonField = "driving_licence"
    , fields =
        [
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = [["usa","alabama"],["usa","alaska"],["usa","arizona"],["usa", "arkansas"],["usa","california"],["usa","colorado"],["usa","connecticut"],["usa","delaware"],["usa","district_of_columbia"],["usa","florida"],["usa","georgia"],["usa","hawaii"],["usa","idaho"],["usa","illinois"],["usa","indiana"], ["usa","iowa"],["usa","kansas"],["usa","kentucky"],["usa","louisiana"],["usa","maine"],["usa","maryland"],["usa","massachusetts"],["usa","michigan"],["usa","minnesota"],["usa","mississippi"],["usa","missouri"],["usa","montana"],["usa","nebraska"],["usa","nevada"],["usa","new_hampshire"],["usa","new_jersey"],["usa","new_mexico"],["usa","new_york"],["usa","north_carolina"],["usa","ohio"],["usa","oklahoma"],["usa","oregon"],["usa","pennsylvania"],["usa","rhode_island"],["usa","south_carolina"],["usa","south_dakota"],["usa","tennessee"],["usa","texas"],["usa","utah"],["usa","vermont"],["usa","virginia"],["usa","washington"],["usa","west_virginia"],["usa","wisconsin"],["usa","wyoming"]]
    }

drone :: ModuleInfo
drone =
  ModuleInfo
    { mmoduleName = "drone"
    , jsonField = "drone"
    , fields =
        [ "name", "battery_type","iso","photo_format", "video_format", "max_shutter_speed","min_shutter_speed", "shutter_speed_units"
        ]
    , unresolvedFields = ["weight", "max_ascent_speed", "max_descent_speed", "flight_time", "max_altitude", "max_flight_distance", "max_speed", "max_wind_resistance", "max_angular_velocity", "max_tilt_angle", "operating_temperature", "battery_capacity", "battery_voltage", "battery_weight", "charging_temperature", "max_charging_power", "max_resolution"]
    , nestedFields = []
    , unresolvedNestedFields = []
    }

minecraft :: ModuleInfo
minecraft =
  ModuleInfo
    { mmoduleName = "minecraft"
    , jsonField = "minecraft"
    , fields =
        [ "blocks","items", "mobs"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }    

prince :: ModuleInfo
prince =
  ModuleInfo
    { mmoduleName = "prince"
    , jsonField = "prince"
    , fields =
        [ "lyric","song", "album","band"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }    

rush :: ModuleInfo
rush =
  ModuleInfo
    { mmoduleName = "rush"
    , jsonField = "rush"
    , fields =
        [ "players","albums"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    } 
   
streetFighter :: ModuleInfo
streetFighter =
  ModuleInfo
    { mmoduleName = "streetFighter"
    , jsonField = "street_fighter"
    , fields =
        [ "characters","stages", "quotes", "moves"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    } 

studioGhibli :: ModuleInfo
studioGhibli =
  ModuleInfo
    { mmoduleName = "studioGhibli"
    , jsonField = "studio_ghibli"
    , fields =
        [ "characters", "quotes", "movies"
        ]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    } 

barcode :: ModuleInfo
barcode =
  ModuleInfo
    { mmoduleName = "barcode"
    , jsonField = "barcode"
    , fields =
        [ 
        ]
    , unresolvedFields = ["ean_8","ean_13","upc_a","upc_e","composite_symbol", "isbn", "ismn","issn"]
    , nestedFields = []
    , unresolvedNestedFields = []
    } 
      
currentOne :: ModuleInfo
currentOne = streetFighter
