{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Music.PearlJam where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.PearlJam
import Faker.TH

$(generateFakeField "pearl_jam" "musicians")

$(generateFakeField "pearl_jam" "albums")

$(generateFakeField "pearl_jam" "songs")



