{-# LANGUAGE TemplateHaskell #-}

module Faker.Music.UmphreysMcgee where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.UmphreysMcgee
import Faker.TH

$(generateFakeField "umphreysMcgee" "song")
