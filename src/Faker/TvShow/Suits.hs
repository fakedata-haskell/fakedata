{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.TvShow.Suits where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Suits
import Faker.TH

$(generateFakeField "suits" "characters")

$(generateFakeField "suits" "quotes")



