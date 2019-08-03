{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Yoda where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Yoda
import Faker.TH

$(generateFakeField "yoda" "quotes")
