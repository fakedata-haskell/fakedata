{-# LANGUAGE TemplateHaskell #-}

module Faker.TvShows.RickAndMorty where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.RickAndMorty
import Faker.TH

$(generateFakeField "rickAndMorty" "characters")

$(generateFakeField "rickAndMorty" "locations")

$(generateFakeField "rickAndMorty" "quotes")
