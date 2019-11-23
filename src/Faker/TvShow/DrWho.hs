{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.TvShow.DrWho where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.DrWho
import Faker.TH

$(generateFakeField "drWho" "character")

$(generateFakeField "drWho" "the_doctors")

$(generateFakeField "drWho" "actors")

$(generateFakeField "drWho" "catch_phrases")

$(generateFakeField "drWho" "quotes")

$(generateFakeField "drWho" "villains")

$(generateFakeField "drWho" "species")
