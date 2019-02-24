{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.VForVendetta where

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

parseVForVendetta :: FromJSON a => FakerSettings -> Value -> Parser a
parseVForVendetta settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  vForVendetta <- faker .: "v_for_vendetta"
  pure vForVendetta
parseVForVendetta settings val = fail $ "expected Object, but got " <> (show val)

parseVForVendettaField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseVForVendettaField settings txt val = do
  vForVendetta <- parseVForVendetta settings val
  field <- vForVendetta .:? txt .!= mempty
  pure field

parseVForVendettaFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseVForVendettaFields settings txts val = do
  vForVendetta <- parseVForVendetta settings val
  helper vForVendetta txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "vForVendetta" "characters")

$(genProvider "vForVendetta" "characters")


$(genParser "vForVendetta" "speeches")

$(genProvider "vForVendetta" "speeches")


$(genParser "vForVendetta" "quotes")

$(genProvider "vForVendetta" "quotes")











