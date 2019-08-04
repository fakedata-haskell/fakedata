{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Relationship where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Relationship
import Faker.TH

$(generateFakeField "relationship" "in_law")

$(generateFakeField "relationship" "spouse")

$(generateFakeField "relationship" "parent")

$(generateFakeField "relationship" "sibling")

$(generateFakeFields "relationship" ["familial", "direct"])

$(generateFakeFields "relationship" ["familial", "extended"])
