{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Music.RockBand where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.RockBand
import Faker.TH

$(generateFakeField "rockBand" "name")

-- | @since 1.0
$(generateFakeField "rockBand" "song")
