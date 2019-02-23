{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.HeyArnold where

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

parseHeyArnold :: FromJSON a => FakerSettings -> Value -> Parser a
parseHeyArnold settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  heyArnold <- faker .: "hey_arnold"
  pure heyArnold
parseHeyArnold settings val = fail $ "expected Object, but got " <> (show val)

parseHeyArnoldField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseHeyArnoldField settings txt val = do
  heyArnold <- parseHeyArnold settings val
  field <- heyArnold .:? txt .!= mempty
  pure field

parseHeyArnoldFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseHeyArnoldFields settings txts val = do
  heyArnold <- parseHeyArnold settings val
  helper heyArnold txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "heyArnold" "characters")

$(genProvider "heyArnold" "characters")


$(genParser "heyArnold" "locations")

$(genProvider "heyArnold" "locations")


$(genParser "heyArnold" "quotes")

$(genProvider "heyArnold" "quotes")











