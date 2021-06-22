{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.TvShow.FreshPrinceOfBelAir where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.FreshPrinceOfBelAir
import Faker.TH

$(generateFakeField "freshPrinceOfBelAir" "characters")

-- | @since 1.0
$(generateFakeField "freshPrinceOfBelAir" "actors")

$(generateFakeField "freshPrinceOfBelAir" "quotes")
