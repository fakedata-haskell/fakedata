{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Creature.Dog where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Dog
import Faker.TH

$(generateFakeField "dog" "name")

$(generateFakeField "dog" "breed")

$(generateFakeField "dog" "sound")

$(generateFakeField "dog" "meme_phrase")

$(generateFakeField "dog" "age")

$(generateFakeField "dog" "coat_length")

$(generateFakeField "dog" "size")
