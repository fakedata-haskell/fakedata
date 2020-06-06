{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Dnd where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Dnd
import Faker.TH

$(generateFakeField "dnd" "species")

$(generateFakeField "dnd" "klasses")

$(generateFakeField "dnd" "backgrounds")

$(generateFakeField "dnd" "alignments")



