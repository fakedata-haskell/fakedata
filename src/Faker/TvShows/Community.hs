{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Community where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Community
import Faker.TH

$(generateFakeField "community" "characters")

$(generateFakeField "community" "quotes")
