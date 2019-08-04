{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Color where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Color
import Faker.TH

$(generateFakeField "color" "name")
