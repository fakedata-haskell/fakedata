{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Hacker where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Hacker
import Faker.TH

$(generateFakeField "hacker" "abbreviation")

$(generateFakeField "hacker" "adjective")

$(generateFakeField "hacker" "noun")

$(generateFakeField "hacker" "verb")

$(generateFakeField "hacker" "ingverb")
