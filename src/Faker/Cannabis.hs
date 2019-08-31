{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Cannabis where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Cannabis
import Faker.TH

$(generateFakeField "cannabis" "strains")

$(generateFakeField "cannabis" "cannabinoid_abbreviations")

$(generateFakeField "cannabis" "cannabinoids")

$(generateFakeField "cannabis" "terpenes")

$(generateFakeField "cannabis" "medical_uses")

$(generateFakeField "cannabis" "health_benefits")

$(generateFakeField "cannabis" "categories")

$(generateFakeField "cannabis" "types")

$(generateFakeField "cannabis" "buzzwords")

-- | @since 0.3.0
$(generateFakeField "cannabis" "brands")
