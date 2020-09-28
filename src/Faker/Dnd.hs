{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Dnd where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Dnd
import Faker.TH

$(generateFakeField "dnd" "klasses")

$(generateFakeField "dnd" "alignments")

$(generateFakeField "dnd" "cities")

$(generateFakeField "dnd" "languages")

$(generateFakeField "dnd" "melee_weapons")

$(generateFakeField "dnd" "monsters")

$(generateFakeField "dnd" "races")

$(generateFakeField "dnd" "ranged_weapons")



