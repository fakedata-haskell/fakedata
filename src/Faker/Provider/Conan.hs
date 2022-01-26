{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Conan where

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


parseConan :: FromJSON a => FakerSettings -> Value -> Parser a
parseConan settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  conan <- faker .: "conan"
  pure conan
parseConan settings val = fail $ "expected Object, but got " <> (show val)

parseConanField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseConanField settings txt val = do
  conan <- parseConan settings val
  field <- conan .:? txt .!= mempty
  pure field

parseConanFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseConanFields settings txts val = do
  conan <- parseConan settings val
  helper conan txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedConanFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [AesonKey]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedConanFields settings txts val = do
  conan <- parseConan settings val
  helper conan txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)




$(genParser "conan" "characters")

$(genProvider "conan" "characters")

$(genParser "conan" "gadgets")

$(genProvider "conan" "gadgets")

$(genParser "conan" "vehicles")

$(genProvider "conan" "vehicles")
