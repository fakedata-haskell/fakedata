{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.TvShow.BigBangTheory where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.BigBangTheory
import Faker.TH

$(generateFakeField "bigBangTheory" "characters")

$(generateFakeField "bigBangTheory" "quotes")



