{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Game.Witcher where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Witcher
import Faker.TH

$(generateFakeField "witcher" "characters")

$(generateFakeField "witcher" "witchers")

$(generateFakeField "witcher" "schools")

$(generateFakeField "witcher" "locations")

$(generateFakeField "witcher" "quotes")

$(generateFakeField "witcher" "monsters")
