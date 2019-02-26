{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Restaurant where

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

parseRestaurant :: FromJSON a => FakerSettings -> Value -> Parser a
parseRestaurant settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  restaurant <- faker .: "restaurant"
  pure restaurant
parseRestaurant settings val = fail $ "expected Object, but got " <> (show val)

parseRestaurantField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseRestaurantField settings txt val = do
  restaurant <- parseRestaurant settings val
  field <- restaurant .:? txt .!= mempty
  pure field

parseRestaurantFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseRestaurantFields settings txts val = do
  restaurant <- parseRestaurant settings val
  helper restaurant txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)


parseUnresolvedRestaurantField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedRestaurantField settings txt val = do
  restaurant <- parseRestaurant settings val
  field <- restaurant .:? txt .!= mempty
  pure $ pure field



$(genParser "restaurant" "name_suffix")

$(genProvider "restaurant" "name_suffix")


$(genParser "restaurant" "type")

$(genProvider "restaurant" "type")


$(genParser "restaurant" "description")

$(genProvider "restaurant" "description")


$(genParser "restaurant" "review")

$(genProvider "restaurant" "review")




$(genParserUnresolved "restaurant" "name_prefix")

$(genProviderUnresolved "restaurant" "name_prefix")

$(genParserUnresolved "restaurant" "name")

$(genProviderUnresolved "restaurant" "name")



resolveRestaurantText :: (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveRestaurantText settings txt = do
  let fields = resolveFields txt
  restaurantFields <- mapM (resolveRestaurantField settings) fields
  pure $ operateFields txt restaurantFields

resolveRestaurantField :: (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text

resolveRestaurantField settings undefined =
  randomUnresolvedVec settings name_prefixProvider resolveName_prefixText

resolveRestaurantField settings undefined =
  randomUnresolvedVec settings nameProvider resolveNameText

resolveRestaurantField settings str = throwM $ InvalidField "restaurant" str






