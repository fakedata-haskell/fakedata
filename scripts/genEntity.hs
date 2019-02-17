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

generateModule :: ModuleInfo -> IO ()
generateModule m@ModuleInfo {..} = do
  let fname = (capitalize moduleName) <.> "hs"
      dat = moduleDef m
  writeFile fname dat

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
