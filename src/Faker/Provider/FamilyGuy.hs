{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.FamilyGuy where

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


parseFamilyGuy :: FromJSON a => FakerSettings -> Value -> Parser a
parseFamilyGuy settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  familyGuy <- faker .: "family_guy"
  pure familyGuy
parseFamilyGuy settings val = fail $ "expected Object, but got " <> (show val)

parseFamilyGuyField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseFamilyGuyField settings txt val = do
  familyGuy <- parseFamilyGuy settings val
  field <- familyGuy .:? txt .!= mempty
  pure field

parseFamilyGuyFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseFamilyGuyFields settings txts val = do
  familyGuy <- parseFamilyGuy settings val
  helper familyGuy txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "familyGuy" "character")

$(genProvider "familyGuy" "character")

$(genParser "familyGuy" "location")

$(genProvider "familyGuy" "location")

$(genParser "familyGuy" "quote")

$(genProvider "familyGuy" "quote")
