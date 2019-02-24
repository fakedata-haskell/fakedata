{-# LANGUAGE TemplateHaskell #-}

module Faker.Verbs where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Verbs
import Faker.TH


$(generateFakeField "verbs" "base")

$(generateFakeField "verbs" "past")

$(generateFakeField "verbs" "past_participle")

$(generateFakeField "verbs" "simple_present")

$(generateFakeField "verbs" "ing_form")






