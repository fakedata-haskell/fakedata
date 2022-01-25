{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Room where

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
import qualified Data.Aeson.Key as K

parseRoom :: FromJSON a => FakerSettings -> Value -> Parser a
parseRoom settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  room <- faker .: "room"
  pure room
parseRoom settings val = fail $ "expected Object, but got " <> (show val)

parseRoomField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseRoomField settings txt val = do
  room <- parseRoom settings val
  field <- room .:? txt .!= mempty
  pure field

parseRoomFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseRoomFields settings txts val = do
  room <- parseRoom settings val
  helper room txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedRoomFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [K.Key]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedRoomFields settings txts val = do
  room <- parseRoom settings val
  helper room txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)




$(genParser "room" "actors")

$(genProvider "room" "actors")

$(genParser "room" "characters")

$(genProvider "room" "characters")

$(genParser "room" "locations")

$(genProvider "room" "locations")

$(genParser "room" "quotes")

$(genProvider "room" "quotes")
