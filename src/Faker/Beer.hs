module Faker.Beer where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Beer

name :: Fake Text
name = Fake $ resolver beerNameProvider

brand :: Fake Text
brand = Fake $ resolver beerBrandProvider

hop :: Fake Text
hop = Fake $ resolver beerHopProvider

yeast :: Fake Text
yeast = Fake $ resolver beerYeastProvider

malt :: Fake Text
malt = Fake $ resolver beerMaltProvider

style :: Fake Text
style = Fake $ resolver beerStyleProvider
