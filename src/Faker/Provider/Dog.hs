{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Dog where

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

parseDog :: FromJSON a => FakerSettings -> Value -> Parser a
parseDog settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  dog <- faker .: "dog"
  pure dog
parseDog settings val = fail $ "expected Object, but got " <> (show val)

parseDogField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseDogField settings txt val = do
  dog <- parseDog settings val
  field <- dog .:? txt .!= mempty
  pure field

$(genParser "dog" "name")

$(genProvider "dog" "name")

$(genParser "dog" "breed")

$(genProvider "dog" "breed")

$(genParser "dog" "sound")

$(genProvider "dog" "sound")

$(genParser "dog" "meme_phrase")

$(genProvider "dog" "meme_phrase")

$(genParser "dog" "age")

$(genProvider "dog" "age")

$(genParser "dog" "coat_length")

$(genProvider "dog" "coat_length")

$(genParser "dog" "size")

$(genProvider "dog" "size")
