{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.JapaneseMedia.Conan where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Conan
import Faker.TH

$(generateFakeField "conan" "characters")

$(generateFakeField "conan" "gadgets")

$(generateFakeField "conan" "vehicles")
