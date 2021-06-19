{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Minecraft where

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

parseMinecraft :: FromJSON a => FakerSettings -> Value -> Parser a
parseMinecraft settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  games <- faker .: "games"
  minecraft <- games .: "minecraft"
  pure minecraft
parseMinecraft settings val = fail $ "expected Object, but got " <> (show val)

parseMinecraftField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseMinecraftField settings txt val = do
  minecraft <- parseMinecraft settings val
  field <- minecraft .:? txt .!= mempty
  pure field

parseMinecraftFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseMinecraftFields settings txts val = do
  minecraft <- parseMinecraft settings val
  helper minecraft txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedMinecraftFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [Text]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedMinecraftFields settings txts val = do
  minecraft <- parseMinecraft settings val
  helper minecraft txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)





$(genParser "minecraft" "blocks")

$(genProvider "minecraft" "blocks")


$(genParser "minecraft" "items")

$(genProvider "minecraft" "items")


$(genParser "minecraft" "mobs")

$(genProvider "minecraft" "mobs")

$(genParser "minecraft" "achievement")

$(genProvider "minecraft" "achievement")

$(genParser "minecraft" "biome")

$(genProvider "minecraft" "biome")

$(genParser "minecraft" "enchantment")

$(genProvider "minecraft" "enchantment")

$(genParser "minecraft" "game_mode")

$(genProvider "minecraft" "game_mode")

$(genParser "minecraft" "status_effect")

     $(genProvider "minecraft" "status_effect")
