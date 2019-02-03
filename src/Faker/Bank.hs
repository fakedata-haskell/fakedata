module Faker.Bank where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Bank

name :: Fake Text
name = Fake $ resolver bankNameProvider

swiftBic :: Fake Text
swiftBic = Fake $ resolver swiftBicProvider
