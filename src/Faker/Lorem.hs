{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Lorem where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Lorem
import Faker.TH

$(generateFakeField "lorem" "words")

$(generateFakeField "lorem" "supplemental")
