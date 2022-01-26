{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.SuperMario where

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


parseSuperMario :: FromJSON a => FakerSettings -> Value -> Parser a
parseSuperMario settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  games <- faker .: "games"
  superMario <- games .: "super_mario"
  pure superMario
parseSuperMario settings val = fail $ "expected Object, but got " <> (show val)

parseSuperMarioField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseSuperMarioField settings txt val = do
  superMario <- parseSuperMario settings val
  field <- superMario .:? txt .!= mempty
  pure field

parseSuperMarioFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseSuperMarioFields settings txts val = do
  superMario <- parseSuperMario settings val
  helper superMario txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedSuperMarioFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [AesonKey]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedSuperMarioFields settings txts val = do
  superMario <- parseSuperMario settings val
  helper superMario txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)




$(genParser "superMario" "characters")

$(genProvider "superMario" "characters")

$(genParser "superMario" "games")

$(genProvider "superMario" "games")

$(genParser "superMario" "locations")

$(genProvider "superMario" "locations")
