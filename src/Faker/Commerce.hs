{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Commerce where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Commerce
import Faker.TH

$(generateFakeField "commerce" "department")
