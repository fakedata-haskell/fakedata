{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.TvShow.DumbAndDumber where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.DumbAndDumber
import Faker.TH

$(generateFakeField "dumbAndDumber" "actors")

$(generateFakeField "dumbAndDumber" "characters")

$(generateFakeField "dumbAndDumber" "quotes")
