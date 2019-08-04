{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.TvShow.SiliconValley where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.SiliconValley
import Faker.TH

$(generateFakeField "siliconValley" "characters")

$(generateFakeField "siliconValley" "companies")

$(generateFakeField "siliconValley" "quotes")

$(generateFakeField "siliconValley" "apps")

$(generateFakeField "siliconValley" "inventions")

$(generateFakeField "siliconValley" "mottos")

$(generateFakeField "siliconValley" "urls")

$(generateFakeField "siliconValley" "email")
