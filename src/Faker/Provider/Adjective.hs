{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Adjective where

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
import qualified Data.Aeson.Key as K

parseAdjective :: FromJSON a => FakerSettings -> Value -> Parser a
parseAdjective settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  adjective <- faker .: "adjective"
  pure adjective
parseAdjective settings val = fail $ "expected Object, but got " <> (show val)

parseAdjectiveField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseAdjectiveField settings txt val = do
  adjective <- parseAdjective settings val
  field <- adjective .:? txt .!= mempty
  pure field

parseAdjectiveFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseAdjectiveFields settings txts val = do
  adjective <- parseAdjective settings val
  helper adjective txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedAdjectiveFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [K.Key]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedAdjectiveFields settings txts val = do
  adjective <- parseAdjective settings val
  helper adjective txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)




$(genParser "adjective" "positive")

$(genProvider "adjective" "positive")

$(genParser "adjective" "negative")

$(genProvider "adjective" "negative")
