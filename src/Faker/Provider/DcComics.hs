{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.DcComics where

import Config
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


parseDcComics :: FromJSON a => FakerSettings -> Value -> Parser a
parseDcComics settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  dcComics <- faker .: "dc_comics"
  pure dcComics
parseDcComics settings val = fail $ "expected Object, but got " <> (show val)

parseDcComicsField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseDcComicsField settings txt val = do
  dcComics <- parseDcComics settings val
  field <- dcComics .:? txt .!= mempty
  pure field

$(genParser "dcComics" "hero")

$(genProvider "dcComics" "hero")

$(genParser "dcComics" "heroine")

$(genProvider "dcComics" "heroine")

$(genParser "dcComics" "villain")

$(genProvider "dcComics" "villain")

$(genParser "dcComics" "name")

$(genProvider "dcComics" "name")

$(genParser "dcComics" "title")

$(genProvider "dcComics" "title")
