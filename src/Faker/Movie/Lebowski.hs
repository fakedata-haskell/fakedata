{-# LANGUAGE TemplateHaskell #-}

module Faker.Movie.Lebowski where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Lebowski
import Faker.TH

$(generateFakeField "lebowski" "actors")

$(generateFakeField "lebowski" "characters")

$(generateFakeField "lebowski" "quotes")
