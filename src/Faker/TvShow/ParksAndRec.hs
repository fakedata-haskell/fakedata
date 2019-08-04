{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.TvShow.ParksAndRec where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.ParksAndRec
import Faker.TH

$(generateFakeField "parksAndRec" "characters")

$(generateFakeField "parksAndRec" "cities")
