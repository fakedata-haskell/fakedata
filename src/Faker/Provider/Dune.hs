{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Dune where

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

parseDune :: FromJSON a => FakerSettings -> Value -> Parser a
parseDune settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  dune <- faker .: "dune"
  pure dune
parseDune settings val = fail $ "expected Object, but got " <> (show val)

parseDuneField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseDuneField settings txt val = do
  dune <- parseDune settings val
  field <- dune .:? txt .!= mempty
  pure field

parseDuneFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseDuneFields settings txts val = do
  dune <- parseDune settings val
  helper dune txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "dune" "characters")

$(genProvider "dune" "characters")

$(genParser "dune" "titles")

$(genProvider "dune" "titles")

$(genParser "dune" "planets")

$(genProvider "dune" "planets")

$(genParser "dune" "cities")

$(genProvider "dune" "cities")

$(genParsers "dune" ["quotes", "guild_navigator"])

$(genProviders "dune" ["quotes", "guild_navigator"])

$(genParsers "dune" ["quotes", "emperor"])

$(genProviders "dune" ["quotes", "emperor"])

$(genParsers "dune" ["quotes", "paul"])

$(genProviders "dune" ["quotes", "paul"])

$(genParsers "dune" ["quotes", "thufir"])

$(genProviders "dune" ["quotes", "thufir"])

$(genParsers "dune" ["quotes", "jessica"])

$(genProviders "dune" ["quotes", "jessica"])

$(genParsers "dune" ["quotes", "irulan"])

$(genProviders "dune" ["quotes", "irulan"])

$(genParsers "dune" ["quotes", "mohiam"])

$(genProviders "dune" ["quotes", "mohiam"])

$(genParsers "dune" ["quotes", "gurney"])

$(genProviders "dune" ["quotes", "gurney"])

$(genParsers "dune" ["quotes", "leto"])

$(genProviders "dune" ["quotes", "leto"])

$(genParsers "dune" ["quotes", "stilgar"])

$(genProviders "dune" ["quotes", "stilgar"])

$(genParsers "dune" ["quotes", "liet_kynes"])

$(genProviders "dune" ["quotes", "liet_kynes"])

$(genParsers "dune" ["quotes", "pardot_kynes"])

$(genProviders "dune" ["quotes", "pardot_kynes"])

$(genParsers "dune" ["quotes", "baron_harkonnen"])

$(genProviders "dune" ["quotes", "baron_harkonnen"])

$(genParsers "dune" ["quotes", "piter"])

$(genProviders "dune" ["quotes", "piter"])

$(genParsers "dune" ["quotes", "alia"])

$(genProviders "dune" ["quotes", "alia"])

$(genParsers "dune" ["quotes", "mapes"])

$(genProviders "dune" ["quotes", "mapes"])

$(genParsers "dune" ["quotes", "duncan"])

$(genProviders "dune" ["quotes", "duncan"])

$(genParsers "dune" ["quotes", "yueh"])

$(genProviders "dune" ["quotes", "yueh"])

$(genParsers "dune" ["sayings", "bene_gesserit"])

$(genProviders "dune" ["sayings", "bene_gesserit"])

$(genParsers "dune" ["sayings", "fremen"])

$(genProviders "dune" ["sayings", "fremen"])

$(genParsers "dune" ["sayings", "mentat"])

$(genProviders "dune" ["sayings", "mentat"])

$(genParsers "dune" ["sayings", "muaddib"])

$(genProviders "dune" ["sayings", "muaddib"])

$(genParsers "dune" ["sayings", "orange_catholic_bible"])

$(genProviders "dune" ["sayings", "orange_catholic_bible"])
