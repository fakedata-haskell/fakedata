{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.BigBangTheory where

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


parseBigBangTheory :: FromJSON a => FakerSettings -> Value -> Parser a
parseBigBangTheory settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  bigBangTheory <- faker .: "big_bang_theory"
  pure bigBangTheory
parseBigBangTheory settings val = fail $ "expected Object, but got " <> (show val)

parseBigBangTheoryField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseBigBangTheoryField settings txt val = do
  bigBangTheory <- parseBigBangTheory settings val
  field <- bigBangTheory .:? txt .!= mempty
  pure field

parseBigBangTheoryFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseBigBangTheoryFields settings txts val = do
  bigBangTheory <- parseBigBangTheory settings val
  helper bigBangTheory txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "bigBangTheory" "characters")

$(genProvider "bigBangTheory" "characters")


$(genParser "bigBangTheory" "quotes")

$(genProvider "bigBangTheory" "quotes")
