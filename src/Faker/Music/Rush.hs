{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Music.Rush where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Rush
import Faker.TH

$(generateFakeField "rush" "players")

$(generateFakeField "rush" "albums")




