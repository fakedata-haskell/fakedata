{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Movie.Room where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Room
import Faker.TH

$(generateFakeField "room" "actors")

$(generateFakeField "room" "characters")

$(generateFakeField "room" "locations")

$(generateFakeField "room" "quotes")
