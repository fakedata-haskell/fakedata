{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.TvShow.Futurama where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Futurama
import Faker.TH

$(generateFakeField "futurama" "characters")

$(generateFakeField "futurama" "locations")

$(generateFakeField "futurama" "quotes")

$(generateFakeField "futurama" "hermes_catchphrases")




