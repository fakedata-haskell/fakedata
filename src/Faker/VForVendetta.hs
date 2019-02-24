{-# LANGUAGE TemplateHaskell #-}

module Faker.VForVendetta where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.VForVendetta
import Faker.TH


$(generateFakeField "vForVendetta" "characters")

$(generateFakeField "vForVendetta" "speeches")

$(generateFakeField "vForVendetta" "quotes")






