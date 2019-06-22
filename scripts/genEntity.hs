#!/usr/bin/env stack
-- stack script --resolver lts-12.7
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Char (toUpper)
import Data.String.Interpolate
import FakeModuleInfo
import System.FilePath ((<.>))

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = (toUpper x) : xs

moduleDef :: ModuleInfo -> String
moduleDef mod@ModuleInfo {..} =
  let capModname = capitalize mmoduleName
      fakeField :: String -> String
      fakeField str =
        [i|
$(generateFakeField "#{mmoduleName}" "#{str}")
|]
      fakeFields :: String
      fakeFields = concat $ map fakeField fields
      ufakeField :: String -> String
      ufakeField str =
        [i|
$(generateFakeFieldUnresolved "#{mmoduleName}" "#{str}")
|]
      ufakeFields :: String
      ufakeFields = concat $ map ufakeField unresolvedFields
      nfakeField :: [String] -> String
      nfakeField str =
        [i|
$(generateFakeFields "#{mmoduleName}" #{show str})
|]
      nfakeFields :: String
      nfakeFields = concat $ map nfakeField nestedFields
      importLine :: String
      importLine = [i|import Faker.Provider.#{capModname}|]
   in [i|{-# LANGUAGE TemplateHaskell #-}
module Faker.#{capModname} where
import Data.Text
import Faker
import Faker.Internal
#{importLine}
import Faker.TH
#{fakeFields}
#{ufakeFields}
#{nfakeFields}
|]

generateModule :: ModuleInfo -> IO ()
generateModule m@ModuleInfo {..} = do
  let fname = (capitalize mmoduleName) <.> "hs"
      dat = moduleDef m
  writeFile ("../src/Faker/" <> fname) dat

main :: IO ()
main = generateModule currentOne
