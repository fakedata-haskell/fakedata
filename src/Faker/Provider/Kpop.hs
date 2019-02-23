{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Kpop where

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

parseKpop :: FromJSON a => FakerSettings -> Value -> Parser a
parseKpop settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  kpop <- faker .: "kpop"
  pure kpop
parseKpop settings val = fail $ "expected Object, but got " <> (show val)

parseKpopField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseKpopField settings txt val = do
  kpop <- parseKpop settings val
  field <- kpop .:? txt .!= mempty
  pure field

parseKpopFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseKpopFields settings txts val = do
  kpop <- parseKpop settings val
  helper kpop txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "kpop" "i_groups")

$(genProvider "kpop" "i_groups")


$(genParser "kpop" "ii_groups")

$(genProvider "kpop" "ii_groups")


$(genParser "kpop" "iii_groups")

$(genProvider "kpop" "iii_groups")


$(genParser "kpop" "girl_groups")

$(genProvider "kpop" "girl_groups")


$(genParser "kpop" "boy_bands")

$(genProvider "kpop" "boy_bands")


$(genParser "kpop" "solo")

$(genProvider "kpop" "solo")











