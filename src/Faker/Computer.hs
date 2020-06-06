{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Computer where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Computer
import Faker.TH

$(generateFakeField "computer" "type")

$(generateFakeField "computer" "platform")



$(generateFakeFields "computer" ["os","linux"])

$(generateFakeFields "computer" ["os","macos"])

$(generateFakeFields "computer" ["os","windows"])

