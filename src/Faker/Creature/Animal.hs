module Faker.Animal where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Animal

name :: Fake Text
name = Fake (\settings -> randomVec settings animalProvider)
