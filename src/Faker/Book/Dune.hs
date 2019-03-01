{-# LANGUAGE TemplateHaskell #-}

module Faker.Book.Dune where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Dune
import Faker.TH

$(generateFakeField "dune" "characters")

$(generateFakeField "dune" "titles")

$(generateFakeField "dune" "planets")

$(generateFakeFields "dune" ["quotes", "guild_navigator"])

$(generateFakeFields "dune" ["quotes", "emperor"])

$(generateFakeFields "dune" ["quotes", "paul"])

$(generateFakeFields "dune" ["quotes", "thufir"])

$(generateFakeFields "dune" ["quotes", "jessica"])

$(generateFakeFields "dune" ["quotes", "irulan"])

$(generateFakeFields "dune" ["quotes", "mohiam"])

$(generateFakeFields "dune" ["quotes", "gurney"])

$(generateFakeFields "dune" ["quotes", "leto"])

$(generateFakeFields "dune" ["quotes", "stilgar"])

$(generateFakeFields "dune" ["quotes", "liet_kynes"])

$(generateFakeFields "dune" ["quotes", "pardot_kynes"])

$(generateFakeFields "dune" ["quotes", "baron_harkonnen"])

$(generateFakeFields "dune" ["quotes", "piter"])

$(generateFakeFields "dune" ["quotes", "alia"])

$(generateFakeFields "dune" ["quotes", "mapes"])

$(generateFakeFields "dune" ["quotes", "duncan"])

$(generateFakeFields "dune" ["quotes", "yueh"])

$(generateFakeFields "dune" ["sayings", "bene_gesserit"])

$(generateFakeFields "dune" ["sayings", "fremen"])

$(generateFakeFields "dune" ["sayings", "mentat"])

$(generateFakeFields "dune" ["sayings", "muaddib"])

$(generateFakeFields "dune" ["sayings", "orange_catholic_bible"])
