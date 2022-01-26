{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Dnd where

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


parseDnd :: FromJSON a => FakerSettings -> Value -> Parser a
parseDnd settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  dnd <- faker .: "dnd"
  pure dnd
parseDnd settings val = fail $ "expected Object, but got " <> (show val)

parseDndField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseDndField settings txt val = do
  dnd <- parseDnd settings val
  field <- dnd .:? txt .!= mempty
  pure field

parseDndFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseDndFields settings txts val = do
  dnd <- parseDnd settings val
  helper dnd txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "dnd" "klasses")

$(genProvider "dnd" "klasses")


$(genParser "dnd" "alignments")

$(genProvider "dnd" "alignments")


$(genParser "dnd" "cities")

$(genProvider "dnd" "cities")


$(genParser "dnd" "languages")

$(genProvider "dnd" "languages")


$(genParser "dnd" "melee_weapons")

$(genProvider "dnd" "melee_weapons")


$(genParser "dnd" "monsters")

$(genProvider "dnd" "monsters")


$(genParser "dnd" "races")

$(genProvider "dnd" "races")


$(genParser "dnd" "ranged_weapons")

$(genProvider "dnd" "ranged_weapons")
