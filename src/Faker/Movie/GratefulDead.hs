{-# LANGUAGE TemplateHaskell #-}

module Faker.Movie.GratefulDead where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.GratefulDead
import Faker.TH

$(generateFakeField "gratefulDead" "players")

$(generateFakeField "gratefulDead" "songs")
