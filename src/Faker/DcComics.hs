{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.DcComics where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.DcComics
import Faker.TH

$(generateFakeField "dcComics" "hero")

$(generateFakeField "dcComics" "heroine")

$(generateFakeField "dcComics" "villain")

$(generateFakeField "dcComics" "name")

$(generateFakeField "dcComics" "title")
