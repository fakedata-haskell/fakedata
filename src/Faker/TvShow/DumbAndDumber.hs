{-# LANGUAGE TemplateHaskell #-}

module Faker.TvShows.DumbAndDumber where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.DumbAndDumber
import Faker.TH

$(generateFakeField "dumbAndDumber" "actors")

$(generateFakeField "dumbAndDumber" "characters")

$(generateFakeField "dumbAndDumber" "quotes")
