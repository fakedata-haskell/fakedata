{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Chiquito where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Chiquito
import Faker.TH

$(generateFakeField "chiquito" "expressions")

$(generateFakeField "chiquito" "terms")

$(generateFakeField "chiquito" "sentences")

$(generateFakeField "chiquito" "jokes")



