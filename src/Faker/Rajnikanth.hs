{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Rajnikanth where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Rajnikanth
import Faker.TH

$(generateFakeField "rajnikanth" "joke")



