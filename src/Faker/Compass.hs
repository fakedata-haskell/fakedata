{-# LANGUAGE TemplateHaskell #-}

module Faker.Compass where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Compass
import Faker.TH

$(generateFakeFieldUnresolved "compass" "direction")

-- todo: fix this
-- $(generateFakeFieldUnresolved "compass" "abbreviation")
-- $(generateFakeFieldUnresolved "compass" "azimuth")
$(generateFakeFields "compass" ["cardinal", "word"])

$(generateFakeFields "compass" ["cardinal", "abbreviation"])

$(generateFakeFields "compass" ["cardinal", "azimuth"])

$(generateFakeFields "compass" ["ordinal", "word"])

$(generateFakeFields "compass" ["ordinal", "abbreviation"])

$(generateFakeFields "compass" ["ordinal", "azimuth"])

$(generateFakeFields "compass" ["half-wind", "word"])

$(generateFakeFields "compass" ["half-wind", "abbreviation"])

$(generateFakeFields "compass" ["half-wind", "azimuth"])

$(generateFakeFields "compass" ["quarter-wind", "word"])

$(generateFakeFields "compass" ["quarter-wind", "abbreviation"])

$(generateFakeFields "compass" ["quarter-wind", "azimuth"])
