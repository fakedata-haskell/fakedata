{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Opera where

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

parseOpera :: FromJSON a => FakerSettings -> Value -> Parser a
parseOpera settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  opera <- faker .: "opera"
  pure opera
parseOpera settings val = fail $ "expected Object, but got " <> (show val)

parseOperaField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseOperaField settings txt val = do
  opera <- parseOpera settings val
  field <- opera .:? txt .!= mempty
  pure field

parseOperaFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseOperaFields settings txts val = do
  opera <- parseOpera settings val
  helper opera txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParsers "opera" ["italian", "by_giuseppe_verdi"])

$(genProviders "opera" ["italian", "by_giuseppe_verdi"])

$(genParsers "opera" ["italian", "by_gioacchino_rossini"])

$(genProviders "opera" ["italian", "by_gioacchino_rossini"])

$(genParsers "opera" ["italian", "by_gaetano_donizetti"])

$(genProviders "opera" ["italian", "by_gaetano_donizetti"])

$(genParsers "opera" ["italian", "by_vincenzo_bellini"])

$(genProviders "opera" ["italian", "by_vincenzo_bellini"])
