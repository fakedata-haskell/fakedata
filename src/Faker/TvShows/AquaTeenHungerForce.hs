module Faker.TvShows.AquaTeenHungerForce where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.AquaTeenHungerForce

character :: Fake Text
character = Fake $ resolver athfCharacterProvider
