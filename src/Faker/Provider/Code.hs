{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Code where

import Config
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseCode :: FromJSON a => FakerSettings -> Value -> Parser a
parseCode settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  code <- faker .: "code"
  pure code
parseCode settings val = fail $ "expected Object, but got " <> (show val)

parseCodeField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseCodeField settings txt val = do
  code <- parseCode settings val
  field <- code .:? txt .!= mempty
  pure field

$(genParser "code" "asin")

$(genProvider "code" "asin")
