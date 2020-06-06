{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Movie.Departed where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Departed
import Faker.TH

$(generateFakeField "departed" "actors")

$(generateFakeField "departed" "characters")

$(generateFakeField "departed" "quotes")



