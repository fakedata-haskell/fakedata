{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Tea where

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


parseTea :: FromJSON a => FakerSettings -> Value -> Parser a
parseTea settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  tea <- faker .: "tea"
  pure tea
parseTea settings val = fail $ "expected Object, but got " <> (show val)

parseTeaField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseTeaField settings txt val = do
  tea <- parseTea settings val
  field <- tea .:? txt .!= mempty
  pure field

parseTeaFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseTeaFields settings txts val = do
  tea <- parseTea settings val
  helper tea txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedTeaFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [AesonKey]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedTeaFields settings txts val = do
  tea <- parseTea settings val
  helper tea txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)




$(genParser "tea" "type")

$(genProvider "tea" "type")







$(genParsers "tea" ["variety","black"])
$(genProviders "tea" ["variety","black"])

$(genParsers "tea" ["variety","oolong"])
$(genProviders "tea" ["variety","oolong"])

$(genParsers "tea" ["variety","green"])
$(genProviders "tea" ["variety","green"])

$(genParsers "tea" ["variety","white"])
$(genProviders "tea" ["variety","white"])

$(genParsers "tea" ["variety","herbal"])
$(genProviders "tea" ["variety","herbal"])
