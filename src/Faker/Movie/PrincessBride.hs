{-# LANGUAGE TemplateHaskell #-}

module Faker.Movie.PrincessBride where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.PrincessBride
import Faker.TH

$(generateFakeField "princessBride" "characters")

$(generateFakeField "princessBride" "quotes")
