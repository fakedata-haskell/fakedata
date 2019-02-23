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

moduleData :: ModuleInfo -> String
moduleData mod@ModuleInfo {..} =
  [i|{-# LANGUAGE OverloadedStrings #-}
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

generateModule :: ModuleInfo -> IO ()
generateModule m@ModuleInfo {..} = do
  let fname = (capitalize moduleName) <.> "hs"
      dat = moduleData m
  writeFile ("../src/Faker/Provider/" <> fname) dat

generateModules :: [ModuleInfo] -> IO ()
generateModules xs = mapM_ generateModule xs

main :: IO ()
main = generateModule currentOne
-- main = mapM_ generateModule fields -- dumbAndDumberInfo
