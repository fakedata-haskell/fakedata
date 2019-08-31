{-# LANGUAGE OverloadedStrings #-}

module Faker.TvShow.AquaTeenHungerForce where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.AquaTeenHungerForce

character :: Fake Text
character =
  Fake $ cachedRandomVec "aquaTeenHungerForce" "character" athfCharacterProvider
