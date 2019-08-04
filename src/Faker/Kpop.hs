{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Kpop where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Kpop
import Faker.TH

$(generateFakeField "kpop" "i_groups")

$(generateFakeField "kpop" "ii_groups")

$(generateFakeField "kpop" "iii_groups")

$(generateFakeField "kpop" "girl_groups")

$(generateFakeField "kpop" "boy_bands")

$(generateFakeField "kpop" "solo")
