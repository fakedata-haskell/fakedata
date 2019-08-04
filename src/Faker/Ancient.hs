{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Ancient where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Ancient
import Faker.TH

$(generateFakeField "ancient" "god")

$(generateFakeField "ancient" "primordial")

$(generateFakeField "ancient" "hero")

$(generateFakeField "ancient" "titan")



