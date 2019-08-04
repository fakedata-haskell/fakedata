{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.JapaneseMedia.DragonBall where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.DragonBall
import Faker.TH

$(generateFakeField "dragonBall" "characters")
