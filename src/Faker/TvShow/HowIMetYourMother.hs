{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.TvShow.HowIMetYourMother where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.HowIMetYourMother
import Faker.TH

$(generateFakeField "howIMetYourMother" "character")

$(generateFakeField "howIMetYourMother" "catch_phrase")

$(generateFakeField "howIMetYourMother" "high_five")

$(generateFakeField "howIMetYourMother" "quote")
