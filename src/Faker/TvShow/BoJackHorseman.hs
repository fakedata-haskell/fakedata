{-# LANGUAGE OverloadedStrings #-}

module Faker.TvShow.BoJackHorseman where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.BoJackHorseman

character :: Fake Text
character =
  Fake $
  cachedRandomVec "boJackHorseman" "character" boJackHorsemanCharacterProvider

quote :: Fake Text
quote =
  Fake $ cachedRandomVec "boJackHorseman" "quote" boJackHorsemanQuoteProvider

tongueTwister :: Fake Text
tongueTwister =
  Fake $
  cachedRandomVec "boJackHorseman" "quote" boJackHorsemanTongueTwisterProvider
