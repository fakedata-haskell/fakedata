{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Beer where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Beer
import Faker.TH

$(generateFakeField "beer" "name")

$(generateFakeField "beer" "brand")

$(generateFakeField "beer" "hop")

$(generateFakeField "beer" "yeast")

$(generateFakeField "beer" "malt")

$(generateFakeField "beer" "style")
