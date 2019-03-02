{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.HitchhikersGuideToTheGalaxy where

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

parseHitchhikersGuideToTheGalaxy ::
     FromJSON a => FakerSettings -> Value -> Parser a
parseHitchhikersGuideToTheGalaxy settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  hitchhikersGuideToTheGalaxy <- faker .: "hitchhikers_guide_to_the_galaxy"
  pure hitchhikersGuideToTheGalaxy
parseHitchhikersGuideToTheGalaxy settings val =
  fail $ "expected Object, but got " <> (show val)

parseHitchhikersGuideToTheGalaxyField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseHitchhikersGuideToTheGalaxyField settings txt val = do
  hitchhikersGuideToTheGalaxy <- parseHitchhikersGuideToTheGalaxy settings val
  field <- hitchhikersGuideToTheGalaxy .:? txt .!= mempty
  pure field

parseHitchhikersGuideToTheGalaxyFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseHitchhikersGuideToTheGalaxyFields settings txts val = do
  hitchhikersGuideToTheGalaxy <- parseHitchhikersGuideToTheGalaxy settings val
  helper hitchhikersGuideToTheGalaxy txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "hitchhikersGuideToTheGalaxy" "characters")

$(genProvider "hitchhikersGuideToTheGalaxy" "characters")

$(genParser "hitchhikersGuideToTheGalaxy" "locations")

$(genProvider "hitchhikersGuideToTheGalaxy" "locations")

$(genParser "hitchhikersGuideToTheGalaxy" "marvin_quote")

$(genProvider "hitchhikersGuideToTheGalaxy" "marvin_quote")

$(genParser "hitchhikersGuideToTheGalaxy" "planets")

$(genProvider "hitchhikersGuideToTheGalaxy" "planets")

$(genParser "hitchhikersGuideToTheGalaxy" "quotes")

$(genProvider "hitchhikersGuideToTheGalaxy" "quotes")

$(genParser "hitchhikersGuideToTheGalaxy" "species")

$(genProvider "hitchhikersGuideToTheGalaxy" "species")

$(genParser "hitchhikersGuideToTheGalaxy" "starships")

$(genProvider "hitchhikersGuideToTheGalaxy" "starships")
