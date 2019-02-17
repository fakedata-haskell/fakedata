#!/usr/bin/env stack
-- stack script --resolver lts-12.7
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Char (toUpper)
import Data.String.Interpolate
import ModuleInfo
import System.FilePath ((<.>))

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

main :: IO ()
main = generateModule currentOne
