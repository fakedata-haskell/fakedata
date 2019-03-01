{-# LANGUAGE TemplateHaskell #-}

module Faker.TvShow.StrangerThings where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.StrangerThings
import Faker.TH

$(generateFakeField "strangerThings" "characters")

$(generateFakeField "strangerThings" "quote")
