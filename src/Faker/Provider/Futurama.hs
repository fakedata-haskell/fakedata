{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Futurama where

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

parseFuturama :: FromJSON a => FakerSettings -> Value -> Parser a
parseFuturama settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  futurama <- faker .: "futurama"
  pure futurama
parseFuturama settings val = fail $ "expected Object, but got " <> (show val)

parseFuturamaField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseFuturamaField settings txt val = do
  futurama <- parseFuturama settings val
  field <- futurama .:? txt .!= mempty
  pure field

parseFuturamaFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseFuturamaFields settings txts val = do
  futurama <- parseFuturama settings val
  helper futurama txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedFuturamaFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [K.Key]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedFuturamaFields settings txts val = do
  futurama <- parseFuturama settings val
  helper futurama txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)





$(genParser "futurama" "characters")

$(genProvider "futurama" "characters")


$(genParser "futurama" "locations")

$(genProvider "futurama" "locations")


$(genParser "futurama" "quotes")

$(genProvider "futurama" "quotes")


$(genParser "futurama" "hermes_catchphrases")

$(genProvider "futurama" "hermes_catchphrases")
