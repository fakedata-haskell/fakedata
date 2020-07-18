{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Coffee where

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

parseCoffee :: FromJSON a => FakerSettings -> Value -> Parser a
parseCoffee settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  coffee <- faker .: "coffee"
  pure coffee
parseCoffee settings val = fail $ "expected Object, but got " <> (show val)

parseCoffeeField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseCoffeeField settings txt val = do
  coffee <- parseCoffee settings val
  field <- coffee .:? txt .!= mempty
  pure field

parseCoffeeFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseCoffeeFields settings txts val = do
  coffee <- parseCoffee settings val
  helper coffee txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

parseUnresolvedCoffeeField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedCoffeeField settings txt val = do
  coffee <- parseCoffee settings val
  field <- coffee .:? txt .!= mempty
  pure $ pure field

$(genParser "coffee" "country")

$(genProvider "coffee" "country")

parseCoffeeRegionsBrazil ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseCoffeeRegionsBrazil settings =
  parseCoffeeFields settings ["regions", "brazil"]

coffeeRegionsBrazilProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
coffeeRegionsBrazilProvider settings =
  fetchData settings Coffee parseCoffeeRegionsBrazil

$(genParser "coffee" "variety")

$(genProvider "coffee" "variety")

$(genParser "coffee" "intensifier")

$(genProvider "coffee" "intensifier")

$(genParser "coffee" "body")

$(genProvider "coffee" "body")

$(genParser "coffee" "descriptor")

$(genProvider "coffee" "descriptor")

parseCoffeeNotes :: FakerSettings -> Value -> Parser (Unresolved Text)
parseCoffeeNotes settings = parseUnresolvedCoffeeField settings "notes"

coffeeNotesProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved Text)
coffeeNotesProvider settings = fetchData settings Coffee parseCoffeeNotes

$(genParser "coffee" "name_1")

$(genProvider "coffee" "name_1")

$(genParser "coffee" "name_2")

$(genProvider "coffee" "name_2")

parseCoffeeBlendName :: FakerSettings -> Value -> Parser (Unresolved Text)
parseCoffeeBlendName settings = parseUnresolvedCoffeeField settings "blend_name"

coffeeBlendNameProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved Text)
coffeeBlendNameProvider settings =
  fetchData settings Coffee parseCoffeeBlendName

$(genParsers "coffee" ["regions", "colombia"])

$(genProviders "coffee" ["regions", "colombia"])

$(genParsers "coffee" ["regions", "sumatra"])

$(genProviders "coffee" ["regions", "sumatra"])

$(genParsers "coffee" ["regions", "ethiopia"])

$(genProviders "coffee" ["regions", "ethiopia"])

$(genParsers "coffee" ["regions", "honduras"])

$(genProviders "coffee" ["regions", "honduras"])

$(genParsers "coffee" ["regions", "kenya"])

$(genProviders "coffee" ["regions", "kenya"])

$(genParsers "coffee" ["regions", "uganda"])

$(genProviders "coffee" ["regions", "uganda"])

$(genParsers "coffee" ["regions", "mexico"])

$(genProviders "coffee" ["regions", "mexico"])

$(genParsers "coffee" ["regions", "guatemala"])

$(genProviders "coffee" ["regions", "guatemala"])

$(genParsers "coffee" ["regions", "nicaragua"])

$(genProviders "coffee" ["regions", "nicaragua"])

$(genParsers "coffee" ["regions", "costa_rica"])

$(genProviders "coffee" ["regions", "costa_rica"])

$(genParsers "coffee" ["regions", "tanzania"])

$(genProviders "coffee" ["regions", "tanzania"])

$(genParsers "coffee" ["regions", "el_salvador"])

$(genProviders "coffee" ["regions", "el_salvador"])

$(genParsers "coffee" ["regions", "rwanda"])

$(genProviders "coffee" ["regions", "rwanda"])

$(genParsers "coffee" ["regions", "burundi"])

$(genProviders "coffee" ["regions", "burundi"])

$(genParsers "coffee" ["regions", "panama"])

$(genProviders "coffee" ["regions", "panama"])

$(genParsers "coffee" ["regions", "yemen"])

$(genProviders "coffee" ["regions", "yemen"])

$(genParsers "coffee" ["regions", "india"])

$(genProviders "coffee" ["regions", "india"])

resolveCoffeeText ::
     (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveCoffeeText = genericResolver' resolveCoffeeText

resolveCoffeeField ::
     (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveCoffeeField settings field@"name_1" =
  cachedRandomVec "coffee" field coffeeName1Provider settings
resolveCoffeeField settings field@"name_2" =
  cachedRandomVec "coffee" field coffeeName2Provider settings
resolveCoffeeField settings field@"intensifier" =
  cachedRandomVec "coffee" field coffeeIntensifierProvider settings
resolveCoffeeField settings field@"body" =
  cachedRandomVec "coffee" field coffeeBodyProvider settings
resolveCoffeeField settings field@"descriptor" =
  cachedRandomVec "coffee" field coffeeDescriptorProvider settings
resolveCoffeeField settings str = throwM $ InvalidField "coffee" str
