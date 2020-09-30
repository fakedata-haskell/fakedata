{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.TvShow.Community where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Community
import Faker.TH

$(generateFakeField "community" "characters")

$(generateFakeField "community" "quotes")
