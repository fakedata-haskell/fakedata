{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Volleyball where

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

parseVolleyball :: FromJSON a => FakerSettings -> Value -> Parser a
parseVolleyball settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  volleyball <- faker .: "volleyball"
  pure volleyball
parseVolleyball settings val = fail $ "expected Object, but got " <> (show val)

parseVolleyballField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseVolleyballField settings txt val = do
  volleyball <- parseVolleyball settings val
  field <- volleyball .:? txt .!= mempty
  pure field

parseVolleyballFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseVolleyballFields settings txts val = do
  volleyball <- parseVolleyball settings val
  helper volleyball txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedVolleyballFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [Text]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedVolleyballFields settings txts val = do
  volleyball <- parseVolleyball settings val
  helper volleyball txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)




$(genParser "volleyball" "team")

$(genProvider "volleyball" "team")

$(genParser "volleyball" "player")

$(genProvider "volleyball" "player")

$(genParser "volleyball" "coach")

$(genProvider "volleyball" "coach")

$(genParser "volleyball" "position")

$(genProvider "volleyball" "position")

$(genParser "volleyball" "formation")

$(genProvider "volleyball" "formation")








