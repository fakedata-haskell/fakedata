{-# LANGUAGE TemplateHaskell #-}

module Faker.HitchhikersGuideToTheGalaxy where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.HitchhikersGuideToTheGalaxy
import Faker.TH


$(generateFakeField "hitchhikersGuideToTheGalaxy" "characters")

$(generateFakeField "hitchhikersGuideToTheGalaxy" "locations")

$(generateFakeField "hitchhikersGuideToTheGalaxy" "marvin_quote")

$(generateFakeField "hitchhikersGuideToTheGalaxy" "planets")

$(generateFakeField "hitchhikersGuideToTheGalaxy" "quotes")

$(generateFakeField "hitchhikersGuideToTheGalaxy" "species")

$(generateFakeField "hitchhikersGuideToTheGalaxy" "starships")






