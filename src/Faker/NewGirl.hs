{-# LANGUAGE TemplateHaskell #-}

module Faker.NewGirl where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.NewGirl
import Faker.TH


$(generateFakeField "newGirl" "characters")

$(generateFakeField "newGirl" "quotes")






