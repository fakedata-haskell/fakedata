{-# LANGUAGE TemplateHaskell #-}

module Faker.Movie.Ghostbusters where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Ghostbusters
import Faker.TH

$(generateFakeField "ghostbusters" "actors")

$(generateFakeField "ghostbusters" "characters")

$(generateFakeField "ghostbusters" "quotes")
