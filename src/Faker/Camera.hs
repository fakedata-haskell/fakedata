{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Camera where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Camera
import Faker.TH

$(generateFakeField "camera" "brand")

$(generateFakeField "camera" "model")

$(generateFakeField "camera" "brand_with_model")




