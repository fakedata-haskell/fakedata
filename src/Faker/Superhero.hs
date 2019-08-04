{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Superhero where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Superhero
import Faker.TH

$(generateFakeField "superhero" "power")

$(generateFakeField "superhero" "prefix")

$(generateFakeField "superhero" "suffix")

$(generateFakeField "superhero" "descriptor")

$(generateFakeFieldUnresolved "superhero" "name")
