{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @since 0.2.0
module Faker.Construction where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Construction
import Faker.TH

$(generateFakeField "construction" "materials")

$(generateFakeField "construction" "subcontract_categories")

$(generateFakeField "construction" "heavy_equipment")

$(generateFakeField "construction" "roles")

$(generateFakeField "construction" "trades")

$(generateFakeField "construction" "standard_cost_codes")
