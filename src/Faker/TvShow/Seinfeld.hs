{-# LANGUAGE TemplateHaskell #-}

module Faker.TvShows.Seinfeld where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Seinfeld
import Faker.TH

$(generateFakeField "seinfeld" "character")

$(generateFakeField "seinfeld" "quote")

$(generateFakeField "seinfeld" "business")
