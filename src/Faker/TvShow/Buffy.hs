{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.TvShow.Buffy where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Buffy
import Faker.TH

$(generateFakeField "buffy" "characters")

$(generateFakeField "buffy" "quotes")

-- | @since 1.0
$(generateFakeField "buffy" "actors")

$(generateFakeField "buffy" "big_bads")

$(generateFakeField "buffy" "episodes")
