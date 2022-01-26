{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Show where

import Config
import Control.Monad.Catch
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH


parseShow :: FromJSON a => FakerSettings -> Value -> Parser a
parseShow settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  show <- faker .: "show"
  pure show
parseShow settings val = fail $ "expected Object, but got " <> (show val)

parseShowField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseShowField settings txt val = do
  show <- parseShow settings val
  field <- show .:? txt .!= mempty
  pure field

parseShowFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseShowFields settings txts val = do
  show <- parseShow settings val
  helper show txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "show" "adult_musical")

$(genProvider "show" "adult_musical")
