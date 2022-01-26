{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Internet where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH


parseInternet :: FromJSON a => FakerSettings -> Value -> Parser a
parseInternet settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  internet <- faker .: "internet"
  pure internet
parseInternet settings val = fail $ "expected Object, but got " <> (show val)

parseInternetField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseInternetField settings txt val = do
  internet <- parseInternet settings val
  field <- internet .:? txt .!= mempty
  pure field

parseInternetFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseInternetFields settings txts val = do
  internet <- parseInternet settings val
  helper internet txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "internet" "free_email")

$(genProvider "internet" "free_email")

$(genParser "internet" "domain_suffix")

$(genProvider "internet" "domain_suffix")

$(genParsers "internet" ["user_agent", "aol"])

$(genProviders "internet" ["user_agent", "aol"])

$(genParsers "internet" ["user_agent", "chrome"])

$(genProviders "internet" ["user_agent", "chrome"])

$(genParsers "internet" ["user_agent", "firefox"])

$(genProviders "internet" ["user_agent", "firefox"])

$(genParsers "internet" ["user_agent", "internet_explorer"])

$(genProviders "internet" ["user_agent", "internet_explorer"])

$(genParsers "internet" ["user_agent", "netscape"])

$(genProviders "internet" ["user_agent", "netscape"])

$(genParsers "internet" ["user_agent", "opera"])

$(genProviders "internet" ["user_agent", "opera"])

$(genParsers "internet" ["user_agent", "safari"])

$(genProviders "internet" ["user_agent", "safari"])
