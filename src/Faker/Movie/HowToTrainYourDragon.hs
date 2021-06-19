{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Movie.HowToTrainYourDragon where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.HowToTrainYourDragon
import Faker.TH

$(generateFakeField "howToTrainYourDragon" "characters")

$(generateFakeField "howToTrainYourDragon" "dragons")

$(generateFakeField "howToTrainYourDragon" "locations")
