{-# LANGUAGE TemplateHaskell #-}

module Faker.TvShow.FreshPrinceOfBelAir where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.FreshPrinceOfBelAir
import Faker.TH

$(generateFakeField "freshPrinceOfBelAir" "characters")

$(generateFakeField "freshPrinceOfBelAir" "celebrities")

$(generateFakeField "freshPrinceOfBelAir" "quotes")
