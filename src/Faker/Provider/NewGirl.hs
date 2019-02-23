{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.NewGirl where

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

parseNewGirl :: FromJSON a => FakerSettings -> Value -> Parser a
parseNewGirl settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  newGirl <- faker .: "new_girl"
  pure newGirl
parseNewGirl settings val = fail $ "expected Object, but got " <> (show val)

parseNewGirlField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseNewGirlField settings txt val = do
  newGirl <- parseNewGirl settings val
  field <- newGirl .:? txt .!= mempty
  pure field

parseNewGirlFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseNewGirlFields settings txts val = do
  newGirl <- parseNewGirl settings val
  helper newGirl txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "newGirl" "characters")

$(genProvider "newGirl" "characters")


$(genParser "newGirl" "quotes")

$(genProvider "newGirl" "quotes")











