{-# LANGUAGE TemplateHaskell #-}

module Faker.TvShow.HeyArnold where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.HeyArnold
import Faker.TH

$(generateFakeField "heyArnold" "characters")

$(generateFakeField "heyArnold" "locations")

$(generateFakeField "heyArnold" "quotes")
