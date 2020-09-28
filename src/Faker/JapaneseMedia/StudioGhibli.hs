{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.JapaneseMedia.StudioGhibli where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.StudioGhibli
import Faker.TH

$(generateFakeField "studioGhibli" "characters")

$(generateFakeField "studioGhibli" "quotes")

$(generateFakeField "studioGhibli" "movies")




