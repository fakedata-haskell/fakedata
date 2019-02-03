module Faker.Appliance where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Appliance

brand :: Fake Text
brand = Fake $ resolver applianceBrandProvider

appliance :: Fake Text
appliance = Fake $ resolver applianceBrandProvider
