{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.StreetFighter where

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


parseStreetFighter :: FromJSON a => FakerSettings -> Value -> Parser a
parseStreetFighter settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  games <- faker .: "games"
  streetFighter <- games .: "street_fighter"
  pure streetFighter
parseStreetFighter settings val = fail $ "expected Object, but got " <> (show val)

parseStreetFighterField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseStreetFighterField settings txt val = do
  streetFighter <- parseStreetFighter settings val
  field <- streetFighter .:? txt .!= mempty
  pure field

parseStreetFighterFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseStreetFighterFields settings txts val = do
  streetFighter <- parseStreetFighter settings val
  helper streetFighter txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedStreetFighterFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [AesonKey]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedStreetFighterFields settings txts val = do
  streetFighter <- parseStreetFighter settings val
  helper streetFighter txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)





$(genParser "streetFighter" "characters")

$(genProvider "streetFighter" "characters")


$(genParser "streetFighter" "stages")

$(genProvider "streetFighter" "stages")


$(genParser "streetFighter" "quotes")

$(genProvider "streetFighter" "quotes")


$(genParser "streetFighter" "moves")

$(genProvider "streetFighter" "moves")
