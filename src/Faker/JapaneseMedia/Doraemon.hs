{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @since 1.0
module Faker.JapaneseMedia.Doraemon where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Doraemon
import Faker.TH

$(generateFakeField "doraemon" "characters")

$(generateFakeField "doraemon" "gadgets")

$(generateFakeField "doraemon" "locations")
