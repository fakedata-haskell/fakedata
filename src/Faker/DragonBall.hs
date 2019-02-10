{-# LANGUAGE TemplateHaskell #-}

module Faker.DragonBall where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.DragonBall
import Faker.TH

$(generateFakeField "dragonBall" "characters")
