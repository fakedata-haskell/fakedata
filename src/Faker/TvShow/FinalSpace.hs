{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.TvShow.FinalSpace where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.FinalSpace
import Faker.TH

$(generateFakeField "finalSpace" "characters")

$(generateFakeField "finalSpace" "vehicles")

$(generateFakeField "finalSpace" "quotes")
