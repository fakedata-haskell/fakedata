{-# LANGUAGE OverloadedStrings #-}

module Faker.Ancient where

import Config
import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Ancient

god :: Fake Text
god = Fake (cachedRandomVec "ancient" "god" ancientGodProvider)

primordial :: Fake Text
primordial = Fake (\settings -> randomVec settings ancientPrimordialProvider)

titan :: Fake Text
titan = Fake (\settings -> randomVec settings ancientTitanProvider)

hero :: Fake Text
hero = Fake (\settings -> randomVec settings ancientHeroProvider)
