{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Movie.BackToTheFuture where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.BackToTheFuture
import Faker.TH

$(generateFakeField "backToTheFuture" "dates")

$(generateFakeField "backToTheFuture" "characters")

$(generateFakeField "backToTheFuture" "quotes")
