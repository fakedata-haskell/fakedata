{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.TvShow.Simpsons where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Simpsons
import Faker.TH

$(generateFakeField "simpsons" "characters")

$(generateFakeField "simpsons" "locations")

$(generateFakeField "simpsons" "quotes")
