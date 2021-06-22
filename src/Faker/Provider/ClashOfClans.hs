{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.ClashOfClans where

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

parseClashOfClans :: FromJSON a => FakerSettings -> Value -> Parser a
parseClashOfClans settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  games <- faker .: "games"
  clashOfClans <- games .: "clash_of_clans"
  pure clashOfClans
parseClashOfClans settings val = fail $ "expected Object, but got " <> (show val)

parseClashOfClansField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseClashOfClansField settings txt val = do
  clashOfClans <- parseClashOfClans settings val
  field <- clashOfClans .:? txt .!= mempty
  pure field

parseClashOfClansFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseClashOfClansFields settings txts val = do
  clashOfClans <- parseClashOfClans settings val
  helper clashOfClans txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedClashOfClansFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [Text]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedClashOfClansFields settings txts val = do
  clashOfClans <- parseClashOfClans settings val
  helper clashOfClans txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)




$(genParser "clashOfClans" "troops")

$(genProvider "clashOfClans" "troops")

$(genParser "clashOfClans" "ranks")

$(genProvider "clashOfClans" "ranks")

$(genParser "clashOfClans" "defensive_buildings")

$(genProvider "clashOfClans" "defensive_buildings")
