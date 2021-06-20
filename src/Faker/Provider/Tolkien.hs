{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Tolkien where

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

parseTolkien :: FromJSON a => FakerSettings -> Value -> Parser a
parseTolkien settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  tolkien <- faker .: "tolkien"
  pure tolkien
parseTolkien settings val = fail $ "expected Object, but got " <> (show val)

parseTolkienField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseTolkienField settings txt val = do
  tolkien <- parseTolkien settings val
  field <- tolkien .:? txt .!= mempty
  pure field

parseTolkienFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseTolkienFields settings txts val = do
  tolkien <- parseTolkien settings val
  helper tolkien txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedTolkienFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [Text]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedTolkienFields settings txts val = do
  tolkien <- parseTolkien settings val
  helper tolkien txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)




$(genParser "tolkien" "poems")

$(genProvider "tolkien" "poems")

$(genParser "tolkien" "locations")

$(genProvider "tolkien" "locations")

$(genParser "tolkien" "races")

$(genProvider "tolkien" "races")

$(genParser "tolkien" "characters")

$(genProvider "tolkien" "characters")







$(genParsers "tolkien" ["lord_of_the_rings","characters"])
$(genProviders "tolkien" ["lord_of_the_rings","characters"])

$(genParsers "tolkien" ["lord_of_the_rings","locations"])
$(genProviders "tolkien" ["lord_of_the_rings","locations"])

$(genParsers "tolkien" ["lord_of_the_rings","quotes"])
$(genProviders "tolkien" ["lord_of_the_rings","quotes"])

$(genParsers "tolkien" ["hobbit","character"])
$(genProviders "tolkien" ["hobbit","character"])

$(genParsers "tolkien" ["hobbit","thorins_company"])
$(genProviders "tolkien" ["hobbit","thorins_company"])

$(genParsers "tolkien" ["hobbit","quote"])
$(genProviders "tolkien" ["hobbit","quote"])

$(genParsers "tolkien" ["hobbit","location"])
$(genProviders "tolkien" ["hobbit","location"])
