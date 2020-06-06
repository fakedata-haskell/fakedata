{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Chiquito where

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

parseChiquito :: FromJSON a => FakerSettings -> Value -> Parser a
parseChiquito settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  chiquito <- faker .: "chiquito"
  pure chiquito
parseChiquito settings val = fail $ "expected Object, but got " <> (show val)

parseChiquitoField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseChiquitoField settings txt val = do
  chiquito <- parseChiquito settings val
  field <- chiquito .:? txt .!= mempty
  pure field

parseChiquitoFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseChiquitoFields settings txts val = do
  chiquito <- parseChiquito settings val
  helper chiquito txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "chiquito" "expressions")

$(genProvider "chiquito" "expressions")


$(genParser "chiquito" "terms")

$(genProvider "chiquito" "terms")


$(genParser "chiquito" "sentences")

$(genProvider "chiquito" "sentences")


$(genParser "chiquito" "jokes")

$(genProvider "chiquito" "jokes")











