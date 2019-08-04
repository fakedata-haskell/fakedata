{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Hipster where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Hipster
import Faker.TH

$(generateFakeField "hipster" "words")
