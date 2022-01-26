{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.FinalSpace where

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


parseFinalSpace :: FromJSON a => FakerSettings -> Value -> Parser a
parseFinalSpace settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  finalSpace <- faker .: "final_space"
  pure finalSpace
parseFinalSpace settings val = fail $ "expected Object, but got " <> (show val)

parseFinalSpaceField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseFinalSpaceField settings txt val = do
  finalSpace <- parseFinalSpace settings val
  field <- finalSpace .:? txt .!= mempty
  pure field

parseFinalSpaceFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseFinalSpaceFields settings txts val = do
  finalSpace <- parseFinalSpace settings val
  helper finalSpace txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedFinalSpaceFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [AesonKey]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedFinalSpaceFields settings txts val = do
  finalSpace <- parseFinalSpace settings val
  helper finalSpace txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)




$(genParser "finalSpace" "characters")

$(genProvider "finalSpace" "characters")

$(genParser "finalSpace" "vehicles")

$(genProvider "finalSpace" "vehicles")

$(genParser "finalSpace" "quotes")

$(genProvider "finalSpace" "quotes")
