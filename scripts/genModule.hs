#!/usr/bin/env stack
-- stack script --resolver lts-12.7
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Char (toUpper)
import Data.String.Interpolate

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
  #{moduleName} <- faker .: "#{moduleName}"
  pure #{moduleName}
parse#{capModname} settings val = fail $ "expected Object, but got " <> (show val)

parse#{capModname}Field ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parse#{capModname}Field settings txt val = do
  #{moduleName} <- parse#{capModname} settings val
  field <- #{moduleName} .:? txt .!= mempty
  pure field

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

#{resolvedFields}

#{unresolved}

#{unresolverFunction1}


|]
  where
    capModname :: String
    capModname = capitalize moduleName
    capitalize :: String -> String
    capitalize [] = []
    capitalize (x:xs) = (toUpper x) : xs
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
    generateresolveFieldFns :: String
    generateresolveFieldFns = concatMap generateresolveFieldFn unresolvedFields

data ModuleInfo = ModuleInfo
  { moduleName :: String -- eg: buffy
  , fields :: [String]
  , unresolvedFields :: [String]
  } deriving (Show, Eq, Ord)

bookModuleInfo :: ModuleInfo
bookModuleInfo =
  ModuleInfo
    { moduleName = "book"
    , fields = ["title", "publisher", "genre"]
    , unresolvedFields = ["author", "jani"]
    }

main :: IO ()
main = putStrLn $ moduleData bookModuleInfo
