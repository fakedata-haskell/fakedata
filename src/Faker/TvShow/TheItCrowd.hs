{-# LANGUAGE TemplateHaskell #-}

module Faker.TvShow.TheItCrowd where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.TheItCrowd
import Faker.TH

$(generateFakeField "theItCrowd" "actors")

$(generateFakeField "theItCrowd" "characters")

$(generateFakeField "theItCrowd" "emails")

$(generateFakeField "theItCrowd" "quotes")
