{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Naruto where

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


parseNaruto :: FromJSON a => FakerSettings -> Value -> Parser a
parseNaruto settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  naruto <- faker .: "naruto"
  pure naruto
parseNaruto settings val = fail $ "expected Object, but got " <> (show val)

parseNarutoField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseNarutoField settings txt val = do
  naruto <- parseNaruto settings val
  field <- naruto .:? txt .!= mempty
  pure field

parseNarutoFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseNarutoFields settings txts val = do
  naruto <- parseNaruto settings val
  helper naruto txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedNarutoFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [AesonKey]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedNarutoFields settings txts val = do
  naruto <- parseNaruto settings val
  helper naruto txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)




$(genParser "naruto" "characters")

$(genProvider "naruto" "characters")

$(genParser "naruto" "villages")

$(genProvider "naruto" "villages")

$(genParser "naruto" "eyes")

$(genProvider "naruto" "eyes")

$(genParser "naruto" "demons")

$(genProvider "naruto" "demons")
