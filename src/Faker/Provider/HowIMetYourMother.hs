{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.HowIMetYourMother where

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

parseHowIMetYourMother :: FromJSON a => FakerSettings -> Value -> Parser a
parseHowIMetYourMother settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  howIMetYourMother <- faker .: "how_i_met_your_mother"
  pure howIMetYourMother
parseHowIMetYourMother settings val = fail $ "expected Object, but got " <> (show val)

parseHowIMetYourMotherField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseHowIMetYourMotherField settings txt val = do
  howIMetYourMother <- parseHowIMetYourMother settings val
  field <- howIMetYourMother .:? txt .!= mempty
  pure field

parseHowIMetYourMotherFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseHowIMetYourMotherFields settings txts val = do
  howIMetYourMother <- parseHowIMetYourMother settings val
  helper howIMetYourMother txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "howIMetYourMother" "character")

$(genProvider "howIMetYourMother" "character")


$(genParser "howIMetYourMother" "catch_phrase")

$(genProvider "howIMetYourMother" "catch_phrase")


$(genParser "howIMetYourMother" "high_five")

$(genProvider "howIMetYourMother" "high_five")


$(genParser "howIMetYourMother" "quote")

$(genProvider "howIMetYourMother" "quote")











