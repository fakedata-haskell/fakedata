{-# LANGUAGE TemplateHaskell #-}

module Faker.MichaelScott where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.MichaelScott
import Faker.TH


$(generateFakeField "michaelScott" "quotes")






