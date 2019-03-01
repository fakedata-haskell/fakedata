{-# LANGUAGE TemplateHaskell #-}

module Faker.TvShows.ParksAndRec where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.ParksAndRec
import Faker.TH

$(generateFakeField "parksAndRec" "characters")

$(generateFakeField "parksAndRec" "cities")
