{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Ghostbusters where

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

parseGhostbusters :: FromJSON a => FakerSettings -> Value -> Parser a
parseGhostbusters settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  ghostbusters <- faker .: "ghostbusters"
  pure ghostbusters
parseGhostbusters settings val = fail $ "expected Object, but got " <> (show val)

parseGhostbustersField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseGhostbustersField settings txt val = do
  ghostbusters <- parseGhostbusters settings val
  field <- ghostbusters .:? txt .!= mempty
  pure field

parseGhostbustersFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseGhostbustersFields settings txts val = do
  ghostbusters <- parseGhostbusters settings val
  helper ghostbusters txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "ghostbusters" "actors")

$(genProvider "ghostbusters" "actors")


$(genParser "ghostbusters" "characters")

$(genProvider "ghostbusters" "characters")


$(genParser "ghostbusters" "quotes")

$(genProvider "ghostbusters" "quotes")











