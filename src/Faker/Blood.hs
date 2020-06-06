{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Blood where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Blood
import Faker.TH
import Control.Monad.IO.Class

$(generateFakeField "blood" "type")

$(generateFakeField "blood" "rh_factor")

$(generateFakeFieldUnresolved "blood" "group")


