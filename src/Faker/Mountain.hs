{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Mountain where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Mountain
import Faker.TH

$(generateFakeField "mountain" "name")

$(generateFakeField "mountain" "range")




