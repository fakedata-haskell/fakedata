{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Mountain where

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


parseMountain :: FromJSON a => FakerSettings -> Value -> Parser a
parseMountain settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  mountain <- faker .: "mountain"
  pure mountain
parseMountain settings val = fail $ "expected Object, but got " <> (show val)

parseMountainField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseMountainField settings txt val = do
  mountain <- parseMountain settings val
  field <- mountain .:? txt .!= mempty
  pure field

parseMountainFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseMountainFields settings txts val = do
  mountain <- parseMountain settings val
  helper mountain txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedMountainFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [AesonKey]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedMountainFields settings txts val = do
  mountain <- parseMountain settings val
  helper mountain txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)




$(genParser "mountain" "name")

$(genProvider "mountain" "name")

$(genParser "mountain" "range")

$(genProvider "mountain" "range")
