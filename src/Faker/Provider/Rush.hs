{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Rush where

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

parseRush :: FromJSON a => FakerSettings -> Value -> Parser a
parseRush settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  rush <- faker .: "rush"
  pure rush
parseRush settings val = fail $ "expected Object, but got " <> (show val)

parseRushField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseRushField settings txt val = do
  rush <- parseRush settings val
  field <- rush .:? txt .!= mempty
  pure field

parseRushFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseRushFields settings txts val = do
  rush <- parseRush settings val
  helper rush txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedRushFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [Text]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedRushFields settings txts val = do
  rush <- parseRush settings val
  helper rush txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)





$(genParser "rush" "players")

$(genProvider "rush" "players")


$(genParser "rush" "albums")

$(genProvider "rush" "albums")











