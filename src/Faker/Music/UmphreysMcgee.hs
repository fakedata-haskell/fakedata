{-# LANGUAGE TemplateHaskell #-}

module Faker.UmphreysMcgee where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.UmphreysMcgee
import Faker.TH


$(generateFakeField "umphreysMcgee" "song")






