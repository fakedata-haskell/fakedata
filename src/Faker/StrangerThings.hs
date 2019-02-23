{-# LANGUAGE TemplateHaskell #-}

module Faker.StrangerThings where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.StrangerThings
import Faker.TH


$(generateFakeField "strangerThings" "characters")

$(generateFakeField "strangerThings" "quote")






