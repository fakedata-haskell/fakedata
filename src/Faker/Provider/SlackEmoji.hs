{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.SlackEmoji where

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
resolveSlackEmojiField settings "people" =
  randomVec settings slackEmojiPeopleProvider
resolveSlackEmojiField settings "nature" =
  randomVec settings slackEmojiNatureProvider
resolveSlackEmojiField settings "food_and_drink" =
  randomVec settings slackEmojiFoodAndDrinkProvider
resolveSlackEmojiField settings "celebration" =
  randomVec settings slackEmojiCelebrationProvider
resolveSlackEmojiField settings "activity" =
  randomVec settings slackEmojiActivityProvider
resolveSlackEmojiField settings "travel_and_places" =
  randomVec settings slackEmojiTravelAndPlacesProvider
resolveSlackEmojiField settings "objects_and_symbols" =
  randomVec settings slackEmojiObjectsAndSymbolsProvider
resolveSlackEmojiField settings "custom" =
  randomVec settings slackEmojiCustomProvider
resolveSlackEmojiField settings str = throwM $ InvalidField "slackEmoji" str
