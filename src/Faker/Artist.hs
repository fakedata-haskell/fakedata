module Faker.Artist where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Artist

name :: Fake Text
name = Fake $ resolver artistNameProvider
