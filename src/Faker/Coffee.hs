{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Coffee where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Coffee
import Faker.TH

$(generateFakeField "coffee" "country")

$(generateFakeField "coffee" "variety")

$(generateFakeField "coffee" "intensifier")

$(generateFakeField "coffee" "body")

$(generateFakeField "coffee" "descriptor")

notes :: Fake Text
notes =
  Fake
    (cachedRandomUnresolvedVecWithoutVector
       "coffee"
       "notes"
       coffeeNotesProvider
       resolveCoffeeText)

$(generateFakeField "coffee" "name_1")

$(generateFakeField "coffee" "name_2")

blendName :: Fake Text
blendName =
  Fake
    (cachedRandomUnresolvedVecWithoutVector
       "coffee"
       "blendName"
       coffeeBlendNameProvider
       resolveCoffeeText)

regionsColombia :: Fake Text
regionsColombia =
  Fake
    (cachedRandomVec "coffee" "regionsColumbia" coffeeRegionsColombiaProvider)

$(generateFakeFields "coffee" ["regions", "brazil"])

$(generateFakeFields "coffee" ["regions", "sumatra"])

$(generateFakeFields "coffee" ["regions", "ethiopia"])

$(generateFakeFields "coffee" ["regions", "honduras"])

$(generateFakeFields "coffee" ["regions", "kenya"])

$(generateFakeFields "coffee" ["regions", "uganda"])

$(generateFakeFields "coffee" ["regions", "mexico"])

$(generateFakeFields "coffee" ["regions", "guatemala"])

$(generateFakeFields "coffee" ["regions", "nicaragua"])

$(generateFakeFields "coffee" ["regions", "costa_rica"])

$(generateFakeFields "coffee" ["regions", "tanzania"])

$(generateFakeFields "coffee" ["regions", "el_salvador"])

$(generateFakeFields "coffee" ["regions", "rwanda"])

$(generateFakeFields "coffee" ["regions", "burundi"])

$(generateFakeFields "coffee" ["regions", "panama"])

$(generateFakeFields "coffee" ["regions", "yemen"])

$(generateFakeFields "coffee" ["regions", "india"])
