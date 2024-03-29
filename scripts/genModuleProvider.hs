#!/usr/bin/env stack
-- stack script --resolver lts-18.0
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}

import Data.Char (toUpper)
import "interpolate" Data.String.Interpolate
import FakeModuleInfo
import System.FilePath ((<.>))

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = (toUpper x) : xs

-- λ> fixupNestedFields ["hello","world","bye"]
-- "helloWorldBye"
fixupNestedFields :: [String] -> String
fixupNestedFields xs = foldl1 (\a b -> a <> capitalize b) xs

moduleData :: ModuleInfo -> String
moduleData mod@ModuleInfo {..} =
  [i|{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.#{capModname} where

import Config
import Control.Monad.Catch
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Monoid ((<>))
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH
import qualified Data.Aeson.Key as K

parse#{capModname} :: FromJSON a => FakerSettings -> Value -> Parser a
parse#{capModname} settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  #{mmoduleName} <- faker .: "#{jsonField}"
  pure #{mmoduleName}
parse#{capModname} settings val = fail $ "expected Object, but got " <> (show val)

parse#{capModname}Field ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parse#{capModname}Field settings txt val = do
  #{mmoduleName} <- parse#{capModname} settings val
  field <- #{mmoduleName} .:? txt .!= mempty
  pure field

parse#{capModname}Fields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parse#{capModname}Fields settings txts val = do
  #{mmoduleName} <- parse#{capModname} settings val
  helper #{mmoduleName} txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

#{unresolvedParser}

#{unresolvedNested}

#{resolvedFields}

#{unresolvedStr}

#{generateresolveFieldFnsString}

#{generateNestedField1}

#{unresolvedFns2}|]
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
  #{mmoduleName} <- parse#{capModname} settings val
  helper #{mmoduleName} txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)

#{unresolvedFns1}|]
    -- unresolvedNFn :: [String] -> String
    -- unresolvedNFn
    unresolvedFn :: [String] -> String
    unresolvedFn field =
      let fieldName = fixupNestedFields field
      in [i|
$(genParserUnresolveds "#{mmoduleName}" #{show field})
$(genProviderUnresolveds "#{mmoduleName}" #{show field})
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
    capModname = capitalize mmoduleName
    genField :: String -> String
    genField field =
      [i|
$(genParser "#{mmoduleName}" "#{field}")

$(genProvider "#{mmoduleName}" "#{field}")
|]
    genFields :: [String]
    genFields = map genField fields
    resolvedFields :: String
    resolvedFields = foldMap (mempty <>) genFields
    unresolvedField :: String -> String
    unresolvedField field =
      [i|
$(genParserUnresolved "#{mmoduleName}" "#{field}")
$(genProviderUnresolved "#{mmoduleName}" "#{field}")|]
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
resolve#{capModname}Text :: (MonadIO m, MonadThrow m) => FakerSettings -> K.Key -> m Text
resolve#{capModname}Text = genericResolver' resolve#{capModname}Field

resolve#{capModname}Field :: (MonadThrow m, MonadIO m) => FakerSettings -> K.Key -> m Text
#{generateresolveFieldFns}
resolve#{capModname}Field settings str = throwM $ InvalidField "#{mmoduleName}" str|]
    generateresolveFieldFn :: String -> String
    generateresolveFieldFn field =
      let cf = capitalize field
       in [i|
resolve#{capModname}Field settings undefined =
  cacheRandomUnresolvedVec settings #{field}Provider resolve#{cf}Text|]
    generateNestedField1 = concat $ map generateNestedField nestedFields
    generateNestedField :: [String] -> String
    generateNestedField field =
      [i|
$(genParsers "#{mmoduleName}" #{show field})
$(genProviders "#{mmoduleName}" #{show field})
|]
    generateresolveFieldFnsString :: String
    generateresolveFieldFnsString =
      if unresolvedFields == [] && unresolvedNestedFields == []
        then mempty
        else unresolverFunction1
    generateresolveFieldFns :: String
    generateresolveFieldFns = concatMap generateresolveFieldFn unresolvedFields
    unresolvedParser :: String
    unresolvedParser =
      if (unresolvedFields == [] && unresolvedNestedFields == [])
        then mempty
        else [i|
parseUnresolved#{capModname}Field ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolved#{capModname}Field settings txt val = do
  #{mmoduleName} <- parse#{capModname} settings val
  field <- #{mmoduleName} .:? txt .!= mempty
  pure $ pure field
|]

generateModule :: ModuleInfo -> IO ()
generateModule m@ModuleInfo {..} = do
  let fname = (capitalize mmoduleName) <.> "hs"
      dat = moduleData m
  writeFile ("../src/Faker/Provider/" <> fname) dat

generateModules :: [ModuleInfo] -> IO ()
generateModules xs = mapM_ generateModule xs

main :: IO ()
main = generateModule currentOne
-- main = mapM_ generateModule fields -- dumbAndDumberInfo
