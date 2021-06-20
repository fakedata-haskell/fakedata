{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Touhou where

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

parseTouhou :: FromJSON a => FakerSettings -> Value -> Parser a
parseTouhou settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  touhou <- faker .: "touhou"
  pure touhou
parseTouhou settings val = fail $ "expected Object, but got " <> (show val)

parseTouhouField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseTouhouField settings txt val = do
  touhou <- parseTouhou settings val
  field <- touhou .:? txt .!= mempty
  pure field

parseTouhouFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseTouhouFields settings txts val = do
  touhou <- parseTouhou settings val
  helper touhou txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedTouhouFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [Text]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedTouhouFields settings txts val = do
  touhou <- parseTouhou settings val
  helper touhou txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)




$(genParser "touhou" "games")

$(genProvider "touhou" "games")

$(genParser "touhou" "characters")

$(genProvider "touhou" "characters")

$(genParser "touhou" "spell_cards")

$(genProvider "touhou" "spell_cards")

$(genParser "touhou" "locations")

$(genProvider "touhou" "locations")

$(genParser "touhou" "songs")

$(genProvider "touhou" "songs")








