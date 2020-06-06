{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Show where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Show
import Faker.TH

$(generateFakeField "show" "adult_musical")



