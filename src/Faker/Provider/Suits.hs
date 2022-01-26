{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Suits where

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


parseSuits :: FromJSON a => FakerSettings -> Value -> Parser a
parseSuits settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  suits <- faker .: "suits"
  pure suits
parseSuits settings val = fail $ "expected Object, but got " <> (show val)

parseSuitsField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseSuitsField settings txt val = do
  suits <- parseSuits settings val
  field <- suits .:? txt .!= mempty
  pure field

parseSuitsFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseSuitsFields settings txts val = do
  suits <- parseSuits settings val
  helper suits txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "suits" "characters")

$(genProvider "suits" "characters")


$(genParser "suits" "quotes")

$(genProvider "suits" "quotes")
