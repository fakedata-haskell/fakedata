{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Nation where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseNation :: FromJSON a => FakerSettings -> Value -> Parser a
parseNation settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  nation <- faker .: "nation"
  pure nation
parseNation settings val = fail $ "expected Object, but got " <> (show val)

parseNationField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseNationField settings txt val = do
  nation <- parseNation settings val
  field <- nation .:? txt .!= mempty
  pure field

parseNationFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseNationFields settings txts val = do
  nation <- parseNation settings val
  helper nation txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "nation" "nationality")

$(genProvider "nation" "nationality")

$(genParser "nation" "language")

$(genProvider "nation" "language")

$(genParser "nation" "capital_city")

$(genProvider "nation" "capital_city")

parseNationFlagEmoji :: FakerSettings -> Value -> Parser (Vector (Vector Word8))
parseNationFlagEmoji settings = parseNationField settings "flag"

nationFlagEmojiProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector (Vector Word8))
nationFlagEmojiProvider settings =
  fetchData settings Nation parseNationFlagEmoji
