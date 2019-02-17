{-# LANGUAGE TemplateHaskell #-}

module Faker.ElectricalComponents where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.ElectricalComponents
import Faker.TH

$(generateFakeField "electricalComponents" "active")

$(generateFakeField "electricalComponents" "passive")

$(generateFakeField "electricalComponents" "electromechanical")
