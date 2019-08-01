module Faker.Ancient where

import Config
import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Ancient

god :: Fake Text
god =
  Fake
    (\settings -> do
       val <- presentInCache Ancient "god" settings
       case val of
         Nothing -> do
           dat <- ancientGodProvider settings
           insertToCache Ancient "god" settings dat
           randomVec settings (\_ -> pure dat)
         Just vec -> do
           randomVec settings (\_ -> pure vec))

primordial :: Fake Text
primordial = Fake (\settings -> randomVec settings ancientPrimordialProvider)

titan :: Fake Text
titan = Fake (\settings -> randomVec settings ancientTitanProvider)

hero :: Fake Text
hero = Fake (\settings -> randomVec settings ancientHeroProvider)
