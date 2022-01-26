{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.BreakingBad where

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


parseBreakingBad :: FromJSON a => FakerSettings -> Value -> Parser a
parseBreakingBad settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  breakingBad <- faker .: "breaking_bad"
  pure breakingBad
parseBreakingBad settings val = fail $ "expected Object, but got " <> (show val)

parseBreakingBadField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseBreakingBadField settings txt val = do
  breakingBad <- parseBreakingBad settings val
  field <- breakingBad .:? txt .!= mempty
  pure field

$(genParser "breakingBad" "character")

$(genProvider "breakingBad" "character")

$(genParser "breakingBad" "episode")

$(genProvider "breakingBad" "episode")
