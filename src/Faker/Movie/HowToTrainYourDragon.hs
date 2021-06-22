{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @since 1.0
module Faker.Movie.HowToTrainYourDragon where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.HowToTrainYourDragon
import Faker.TH

$(generateFakeField "howToTrainYourDragon" "characters")

$(generateFakeField "howToTrainYourDragon" "dragons")

$(generateFakeField "howToTrainYourDragon" "locations")
