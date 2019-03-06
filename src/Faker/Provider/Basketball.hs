{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Basketball where

import Config
import Control.Monad.Catch
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseBasketball :: FromJSON a => FakerSettings -> Value -> Parser a
parseBasketball settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  basketball <- faker .: "basketball"
  pure basketball
parseBasketball settings val = fail $ "expected Object, but got " <> (show val)

parseBasketballField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseBasketballField settings txt val = do
  basketball <- parseBasketball settings val
  field <- basketball .:? txt .!= mempty
  pure field

parseBasketballFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseBasketballFields settings txts val = do
  basketball <- parseBasketball settings val
  helper basketball txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "basketball" "teams")

$(genProvider "basketball" "teams")


$(genParser "basketball" "players")

$(genProvider "basketball" "players")


$(genParser "basketball" "coaches")

$(genProvider "basketball" "coaches")


$(genParser "basketball" "positions")

$(genProvider "basketball" "positions")











