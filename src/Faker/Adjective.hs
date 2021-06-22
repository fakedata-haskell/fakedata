{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @since 1.0
module Faker.Adjective where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Adjective
import Faker.TH

$(generateFakeField "adjective" "positive")

$(generateFakeField "adjective" "negative")
