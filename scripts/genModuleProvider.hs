#!/usr/bin/env stack
-- stack script --resolver lts-12.7
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Char (toUpper)
import Data.String.Interpolate
import System.FilePath ((<.>))

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

#{unresolvedParser}

#{resolvedFields}

#{unresolvedStr}

#{generateresolveFieldFnsString}

#{generateNestedField1}

|]
  where
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

data ModuleInfo = ModuleInfo
  { moduleName :: String -- eg: buffy
  , jsonField :: String
  , fields :: [String]
  , unresolvedFields :: [String]
  , nestedFields :: [[String]]
  } deriving (Show, Eq, Ord)

bookModuleInfo :: ModuleInfo
bookModuleInfo =
  ModuleInfo
    { moduleName = "book"
    , jsonField = "book"
    , fields = ["title", "publisher", "genre"]
    , unresolvedFields = []
    , nestedFields = []
    }

dumbAndDumberInfo :: ModuleInfo
dumbAndDumberInfo =
  ModuleInfo
    { moduleName = "dumbAndDumber"
    , jsonField = "dumb_and_dumber"
    , fields = ["actors", "characters", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
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
    }

educatorModuleInfo :: ModuleInfo
educatorModuleInfo =
  ModuleInfo
    { moduleName = "educator"
    , jsonField = "educator"
    , fields = ["name", "secondary"]
    , unresolvedFields = []
    , nestedFields = []
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
    }

electricalComponents :: ModuleInfo
electricalComponents =
  ModuleInfo
    { moduleName = "electricalComponents"
    , jsonField = "electrical_components"
    , fields = ["active", "passive", "electromechanical"]
    , unresolvedFields = []
    , nestedFields = []
    }

esport :: ModuleInfo
esport =
  ModuleInfo
    { moduleName = "esport"
    , jsonField = "esport"
    , fields = ["players", "teams", "events", "leagues", "games"]
    , unresolvedFields = []
    , nestedFields = []
    }

fallout :: ModuleInfo
fallout =
  ModuleInfo
    { moduleName = "fallout"
    , jsonField = "fallout"
    , fields = ["characters", "factions", "locations", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    }

familyGuy :: ModuleInfo
familyGuy =
  ModuleInfo
    { moduleName = "familyGuy"
    , jsonField = "family_guy"
    , fields = ["character", "location", "quote"]
    , unresolvedFields = []
    , nestedFields = []
    }

file :: ModuleInfo
file =
  ModuleInfo
    { moduleName = "file"
    , jsonField = "file"
    , fields = ["extension", "mime_type"]
    , unresolvedFields = []
    , nestedFields = []
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
    }

football :: ModuleInfo
football =
  ModuleInfo
    { moduleName = "football"
    , jsonField = "football"
    , fields = ["teams", "players", "coaches", "competitions", "positions"]
    , unresolvedFields = []
    , nestedFields = []
    }

fpba :: ModuleInfo
fpba =
  ModuleInfo
    { moduleName = "freshPrinceOfBelAir"
    , jsonField = "the_fresh_prince_of_bel_air"
    , fields = ["characters", "celebrities", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    }

friends :: ModuleInfo
friends =
  ModuleInfo
    { moduleName = "friends"
    , jsonField = "friends"
    , fields = ["characters", "locations", "quotes"]
    , unresolvedFields = []
    , nestedFields = []
    }

funnyName :: ModuleInfo
funnyName =
  ModuleInfo
    { moduleName = "funnyName"
    , jsonField = "funny_name"
    , fields = ["name"]
    , unresolvedFields = []
    , nestedFields = []
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
    }

main :: IO ()
main = generateModule cofee
-- main = mapM_ generateModule fields -- dumbAndDumberInfo
