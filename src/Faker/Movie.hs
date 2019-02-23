{-# LANGUAGE TemplateHaskell #-}

module Faker.Movie where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Movie
import Faker.TH


$(generateFakeField "movie" "quote")






