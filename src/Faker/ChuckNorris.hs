{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.ChuckNorris where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.ChuckNorris
import Faker.TH

$(generateFakeField "chuckNorris" "fact")
