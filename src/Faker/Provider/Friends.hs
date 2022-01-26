{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Friends where

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


parseFriends :: FromJSON a => FakerSettings -> Value -> Parser a
parseFriends settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  friends <- faker .: "friends"
  pure friends
parseFriends settings val = fail $ "expected Object, but got " <> (show val)

parseFriendsField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseFriendsField settings txt val = do
  friends <- parseFriends settings val
  field <- friends .:? txt .!= mempty
  pure field

parseFriendsFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseFriendsFields settings txts val = do
  friends <- parseFriends settings val
  helper friends txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "friends" "characters")

$(genProvider "friends" "characters")

$(genParser "friends" "locations")

$(genProvider "friends" "locations")

$(genParser "friends" "quotes")

$(genProvider "friends" "quotes")
