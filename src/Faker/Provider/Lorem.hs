{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Lorem where

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

parseLorem :: FromJSON a => FakerSettings -> Value -> Parser a
parseLorem settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  lorem <- faker .: "lorem"
  pure lorem
parseLorem settings val = fail $ "expected Object, but got " <> (show val)

parseLoremField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseLoremField settings txt val = do
  lorem <- parseLorem settings val
  field <- lorem .:? txt .!= mempty
  pure field

parseLoremFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseLoremFields settings txts val = do
  lorem <- parseLorem settings val
  helper lorem txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "lorem" "words")

$(genProvider "lorem" "words")


$(genParser "lorem" "supplemental")

$(genProvider "lorem" "supplemental")











