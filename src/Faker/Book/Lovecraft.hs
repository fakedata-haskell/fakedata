{-# LANGUAGE TemplateHaskell #-}

module Faker.Book.Lovecraft where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Lovecraft
import Faker.TH

$(generateFakeField "lovecraft" "fhtagn")

$(generateFakeField "lovecraft" "deity")

$(generateFakeField "lovecraft" "location")

$(generateFakeField "lovecraft" "tome")

$(generateFakeField "lovecraft" "words")
