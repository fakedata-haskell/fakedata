{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.JapaneseMedia.OnePiece where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.OnePiece
import Faker.TH

$(generateFakeField "onePiece" "characters")

$(generateFakeField "onePiece" "seas")

$(generateFakeField "onePiece" "islands")

$(generateFakeField "onePiece" "locations")

$(generateFakeField "onePiece" "quotes")

$(generateFakeField "onePiece" "akumas_no_mi")
