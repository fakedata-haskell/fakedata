{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Team where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.Address (resolveAddressText, stateProvider, resolveAddressField)
import Faker.Provider.TH
import Language.Haskell.TH

parseTeam :: FromJSON a => FakerSettings -> Value -> Parser a
parseTeam settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  team <- faker .: "team"
  pure team
parseTeam settings val = fail $ "expected Object, but got " <> (show val)

parseTeamField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseTeamField settings txt val = do
  team <- parseTeam settings val
  field <- team .:? txt .!= mempty
  pure field

parseTeamFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseTeamFields settings txts val = do
  team <- parseTeam settings val
  helper team txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

parseUnresolvedTeamField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedTeamField settings txt val = do
  team <- parseTeam settings val
  field <- team .:? txt .!= mempty
  pure $ pure field

$(genParser "team" "creature")

$(genProvider "team" "creature")

$(genParser "team" "sport")

$(genProvider "team" "sport")

$(genParser "team" "mascot")

$(genProvider "team" "mascot")

$(genParser "team" "main_teams")

$(genProvider "team" "main_teams")

$(genParser "team" "gentile")

$(genProvider "team" "gentile")

$(genParser "team" "prefix")

$(genProvider "team" "prefix")

$(genParser "team" "suffix")

$(genProvider "team" "suffix")

$(genParserUnresolved "team" "name")

$(genProviderUnresolved "team" "name")

resolveTeamText :: (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveTeamText settings txt = do
  let fields = resolveFields txt
  teamFields <-
    mapM
      (\(seed, field) -> resolveTeamField (modifyRandomGen settings seed) field)
      (zip [1 ..] fields)
  pure $ operateFields txt teamFields

resolveTeamField :: (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveTeamField settings field@"creature" =
  cachedRandomVec "team" field teamCreatureProvider settings
resolveTeamField settings "Address.state" =
  cachedRandomVec "address" "state" stateProvider settings
resolveTeamField settings "Address.city" =
  resolveAddressField settings "city"
resolveTeamField settings "Team.main_teams" =
  cachedRandomVec "address" "main_teams" teamMainTeamsProvider settings
resolveTeamField settings "Team.prefix" =
  cachedRandomVec "address" "prefix" teamPrefixProvider settings
resolveTeamField settings "suffix" =
  cachedRandomVec "address" "suffix" teamSuffixProvider settings
resolveTeamField settings "Team.gentile" =
  cachedRandomVec "address" "gentile" teamGentileProvider settings
resolveTeamField settings field@"name" =
  cachedRandomUnresolvedVec
    "team"
    field
    teamNameProvider
    resolveTeamText
    settings
resolveTeamField settings str = throwM $ InvalidField "team" str
