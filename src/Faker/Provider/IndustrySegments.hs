{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.IndustrySegments where

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

parseIndustrySegments :: FromJSON a => FakerSettings -> Value -> Parser a
parseIndustrySegments settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  industrySegments <- faker .: "industry_segments"
  pure industrySegments
parseIndustrySegments settings val = fail $ "expected Object, but got " <> (show val)

parseIndustrySegmentsField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseIndustrySegmentsField settings txt val = do
  industrySegments <- parseIndustrySegments settings val
  field <- industrySegments .:? txt .!= mempty
  pure field

parseIndustrySegmentsFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseIndustrySegmentsFields settings txts val = do
  industrySegments <- parseIndustrySegments settings val
  helper industrySegments txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "industrySegments" "industry")

$(genProvider "industrySegments" "industry")


$(genParser "industrySegments" "super_sector")

$(genProvider "industrySegments" "super_sector")


$(genParser "industrySegments" "sector")

$(genProvider "industrySegments" "sector")


$(genParser "industrySegments" "sub_sector")

$(genProvider "industrySegments" "sub_sector")











