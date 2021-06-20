{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Fantasy.Tolkien where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Tolkien
import Faker.TH

$(generateFakeField "tolkien" "poems")

$(generateFakeField "tolkien" "locations")

$(generateFakeField "tolkien" "races")

$(generateFakeField "tolkien" "characters")

$(generateFakeFields "tolkien" ["lord_of_the_rings","characters"])

$(generateFakeFields "tolkien" ["lord_of_the_rings","locations"])

$(generateFakeFields "tolkien" ["lord_of_the_rings","quotes"])

$(generateFakeFields "tolkien" ["hobbit","character"])

$(generateFakeFields "tolkien" ["hobbit","thorins_company"])

$(generateFakeFields "tolkien" ["hobbit","quote"])

$(generateFakeFields "tolkien" ["hobbit","location"])
