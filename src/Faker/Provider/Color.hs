{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Color where

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

parseColor :: FromJSON a => FakerSettings -> Value -> Parser a
parseColor settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  color <- faker .: "color"
  pure color
parseColor settings val = fail $ "expected Object, but got " <> (show val)

parseColorField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseColorField settings txt val = do
  color <- parseColor settings val
  field <- color .:? txt .!= mempty
  pure field

$(genParser "color" "name")

$(genProvider "color" "name")
