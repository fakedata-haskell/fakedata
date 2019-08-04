{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Artist where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Artist
import Faker.TH

$(generateFakeField "artist" "names")



