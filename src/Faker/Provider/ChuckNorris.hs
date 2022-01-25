{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.ChuckNorris where

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
import qualified Data.Aeson.Key as K

parseChuckNorris :: FromJSON a => FakerSettings -> Value -> Parser a
parseChuckNorris settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  chuckNorris <- faker .: "chuck_norris"
  pure chuckNorris
parseChuckNorris settings val = fail $ "expected Object, but got " <> (show val)

parseChuckNorrisField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseChuckNorrisField settings txt val = do
  chuckNorris <- parseChuckNorris settings val
  field <- chuckNorris .:? txt .!= mempty
  pure field

$(genParser "chuckNorris" "fact")

$(genProvider "chuckNorris" "fact")
