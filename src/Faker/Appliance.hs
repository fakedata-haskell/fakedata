{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Appliance where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Appliance
import Faker.TH

$(generateFakeField "appliance" "brand")

$(generateFakeField "appliance" "equipment")
