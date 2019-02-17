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
  , unresolvedNestedFields :: [[String]]
  } deriving (Show, Eq, Ord)

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = (toUpper x) : xs

moduleData :: ModuleInfo -> String
moduleData mod@ModuleInfo {..} =
  [i|
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.#{capModname} where

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

parse#{capModname} :: FromJSON a => FakerSettings -> Value -> Parser a
parse#{capModname} settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  #{moduleName} <- faker .: "#{jsonField}"
  pure #{moduleName}
parse#{capModname} settings val = fail $ "expected Object, but got " <> (show val)

parse#{capModname}Field ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parse#{capModname}Field settings txt val = do
  #{moduleName} <- parse#{capModname} settings val
  field <- #{moduleName} .:? txt .!= mempty
  pure field

parse#{capModname}Fields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parse#{capModname}Fields settings txts val = do
  #{moduleName} <- parse#{capModname} settings val
  helper #{moduleName} txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

#{unresolvedParser}

#{resolvedFields}

#{unresolvedStr}

#{generateresolveFieldFnsString}

#{generateNestedField1}

#{unresolvedFns2}

|]
  where
    unresolvedNested :: String
    unresolvedNested =
      [i|
parseUnresolved#{capModname}Fields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [Text]
  -> Value
  -> Parser (Unresolved a)
parseUnresolved#{capModname}Fields settings txts val = do
  #{moduleName} <- parse#{capModname} settings val
  helper #{moduleName} txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)

#{unresolvedFns1}
|]
    -- unresolvedNFn :: [String] -> String
    -- unresolvedNFn
    unresolvedFn :: [String] -> String
    unresolvedFn field =
      [i|
$(genParserUnresolveds "#{moduleName}" "#{show field}")

$(genProviderUnresolveds "#{moduleName}" "#{show field}")

|]
    unresolvedFns :: [String]
    unresolvedFns = map unresolvedFn unresolvedNestedFields
    unresolvedFns1 :: String
    unresolvedFns1 = foldMap (mempty <>) unresolvedFns
    unresolvedFns2 :: String
    unresolvedFns2 =
      if unresolvedNestedFields == []
        then mempty
        else unresolvedFns1
    capModname :: String
    capModname = capitalize moduleName
    genField :: String -> String
    genField field =
      [i|
$(genParser "#{moduleName}" "#{field}")

$(genProvider "#{moduleName}" "#{field}")

|]
    genFields :: [String]
    genFields = map genField fields
    resolvedFields :: String
    resolvedFields = foldMap (mempty <>) genFields
    unresolvedField :: String -> String
    unresolvedField field =
      [i|
$(genParserUnresolved "#{moduleName}" "#{field}")

$(genProviderUnresolved "#{moduleName}" "#{field}")
|]
    unres :: [String]
    unres = map unresolvedField unresolvedFields
    unresolved :: String
    unresolved = foldMap (mempty <>) unres
    unresolvedStr :: String
    unresolvedStr =
      if unresolvedFields == []
        then mempty
        else unresolved
    unresolverFunction1 :: String
    unresolverFunction1 =
      [i|
resolve#{capModname}Text :: (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolve#{capModname}Text settings txt = do
  let fields = resolveFields txt
  #{moduleName}Fields <- mapM (resolve#{capModname}Field settings) fields
  pure $ operateFields txt #{moduleName}Fields

resolve#{capModname}Field :: (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
#{generateresolveFieldFns}
resolve#{capModname}Field settings str = throwM $ InvalidField "#{moduleName}" str
|]
    generateresolveFieldFn :: String -> String
    generateresolveFieldFn field =
      let cf = capitalize field
       in [i|
resolve#{capModname}Field settings undefined =
  randomUnresolvedVec settings #{field}Provider resolve#{cf}Text
|]
    generateNestedField1 = concat $ map generateNestedField nestedFields
    generateNestedField :: [String] -> String
    generateNestedField field =
      [i|
$(genParsers "#{moduleName}" #{show field})

$(genProviders "#{moduleName}" #{show field})

|]
    generateresolveFieldFnsString :: String
    generateresolveFieldFnsString =
      if unresolvedFields == []
        then mempty
        else unresolverFunction1
    generateresolveFieldFns :: String
    generateresolveFieldFns = concatMap generateresolveFieldFn unresolvedFields
    unresolvedParser :: String
    unresolvedParser =
      if unresolvedFields == []
        then mempty
        else [i|
parseUnresolved#{capModname}Field ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolved#{capModname}Field settings txt val = do
  #{moduleName} <- parse#{capModname} settings val
  field <- #{moduleName} .:? txt .!= mempty
  pure $ pure field
|]

bookModuleInfo :: ModuleInfo
bookModuleInfo =
  ModuleInfo
    { moduleName = "book"
    , jsonField = "book"
    , fields = ["title", "publisher", "genre"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

dumbAndDumberInfo :: ModuleInfo
dumbAndDumberInfo =
  ModuleInfo
    { moduleName = "dumbAndDumber"
    , jsonField = "dumb_and_dumber"
    , fields = ["actors", "characters", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

educator :: ModuleInfo
educator =
  ModuleInfo
    { moduleName = "educator"
    , jsonField = "educator"
    , fields = ["name", "secondary"]
    , nestedFields =
        [ ["tertiary", "type"]
        , ["tertiary", "degree", "subject"]
        , ["tertiary", "degree", "type"]
        ]
    , unresolvedFields = ["hi"]
    , unresolvedNestedFields = []
    }

generateModule :: ModuleInfo -> IO ()
generateModule m@ModuleInfo {..} = do
  let fname = (capitalize moduleName) <.> "hs"
      dat = moduleData m
  writeFile fname dat

generateModules :: [ModuleInfo] -> IO ()
generateModules xs = mapM_ generateModule xs

duneModuleInfo :: ModuleInfo
duneModuleInfo =
  ModuleInfo
    { moduleName = "dune"
    , jsonField = "dune"
    , fields = ["characters", "titles", "planets"]
    , unresolvedFields = []
    , nestedFields = []
    , unresolvedNestedFields = []
    }

educatorModuleInfo :: ModuleInfo
educatorModuleInfo =
  ModuleInfo
    { moduleName = "educator"
    , jsonField = "educator"
    , fields = ["name", "secondary"]
    , unresolvedFields = ["hi"]
    , nestedFields = []
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

files :: [ModuleInfo]
files =
  [ duneModuleInfo
  , elderScrolls
  , electricalComponents
  , esport
  , fallout
  , familyGuy
  , file
  , food
  , football
  , fpba
  , friends
  , funnyName
  ]

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

main :: IO ()
main = generateModule compass
-- main = mapM_ generateModule fields -- dumbAndDumberInfo
