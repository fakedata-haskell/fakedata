{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.BossaNova where

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

parseBossaNova :: FromJSON a => FakerSettings -> Value -> Parser a
parseBossaNova settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  bossaNova <- faker .: "bossa_nova"
  pure bossaNova
parseBossaNova settings val = fail $ "expected Object, but got " <> (show val)

parseBossaNovaField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseBossaNovaField settings txt val = do
  bossaNova <- parseBossaNova settings val
  field <- bossaNova .:? txt .!= mempty
  pure field

$(genParser "bossaNova" "artists")

$(genProvider "bossaNova" "artists")

$(genParser "bossaNova" "songs")

$(genProvider "bossaNova" "songs")
