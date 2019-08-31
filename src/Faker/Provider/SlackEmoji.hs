{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.SlackEmoji where

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
import Faker.Provider.TH
import Language.Haskell.TH

parseSlackEmoji :: FromJSON a => FakerSettings -> Value -> Parser a
parseSlackEmoji settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  slackEmoji <- faker .: "slack_emoji"
  pure slackEmoji
parseSlackEmoji settings val = fail $ "expected Object, but got " <> (show val)

parseSlackEmojiField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseSlackEmojiField settings txt val = do
  slackEmoji <- parseSlackEmoji settings val
  field <- slackEmoji .:? txt .!= mempty
  pure field

parseSlackEmojiFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseSlackEmojiFields settings txts val = do
  slackEmoji <- parseSlackEmoji settings val
  helper slackEmoji txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

parseUnresolvedSlackEmojiField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedSlackEmojiField settings txt val = do
  slackEmoji <- parseSlackEmoji settings val
  field <- slackEmoji .:? txt .!= mempty
  pure $ pure field

$(genParser "slackEmoji" "people")

$(genProvider "slackEmoji" "people")

$(genParser "slackEmoji" "nature")

$(genProvider "slackEmoji" "nature")

$(genParser "slackEmoji" "food_and_drink")

$(genProvider "slackEmoji" "food_and_drink")

$(genParser "slackEmoji" "celebration")

$(genProvider "slackEmoji" "celebration")

$(genParser "slackEmoji" "activity")

$(genProvider "slackEmoji" "activity")

$(genParser "slackEmoji" "travel_and_places")

$(genProvider "slackEmoji" "travel_and_places")

$(genParser "slackEmoji" "objects_and_symbols")

$(genProvider "slackEmoji" "objects_and_symbols")

$(genParser "slackEmoji" "custom")

$(genProvider "slackEmoji" "custom")

$(genParserUnresolved "slackEmoji" "emoji")

$(genProviderUnresolved "slackEmoji" "emoji")

resolveSlackEmojiText ::
     (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveSlackEmojiText settings txt = do
  let fields = resolveFields txt
  slackEmojiFields <- mapM (resolveSlackEmojiField settings) fields
  pure $ operateFields txt slackEmojiFields

resolveSlackEmojiField ::
     (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveSlackEmojiField settings field@"people" =
  cachedRandomVec "slackEmoji" field slackEmojiPeopleProvider settings
resolveSlackEmojiField settings field@"nature" =
  cachedRandomVec "slackEmoji" field slackEmojiNatureProvider settings
resolveSlackEmojiField settings field@"food_and_drink" =
  cachedRandomVec "slackEmoji" field slackEmojiFoodAndDrinkProvider settings
resolveSlackEmojiField settings field@"celebration" =
  cachedRandomVec "slackEmoji" field slackEmojiCelebrationProvider settings
resolveSlackEmojiField settings field@"activity" =
  cachedRandomVec "slackEmoji" field slackEmojiActivityProvider settings
resolveSlackEmojiField settings field@"travel_and_places" =
  cachedRandomVec "slackEmoji" field slackEmojiTravelAndPlacesProvider settings
resolveSlackEmojiField settings field@"objects_and_symbols" =
  cachedRandomVec
    "slackEmoji"
    field
    slackEmojiObjectsAndSymbolsProvider
    settings
resolveSlackEmojiField settings field@"custom" =
  cachedRandomVec "slackEmoji" field slackEmojiCustomProvider settings
resolveSlackEmojiField settings str = throwM $ InvalidField "slackEmoji" str
