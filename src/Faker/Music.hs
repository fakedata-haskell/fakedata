{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Music where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Music
import Faker.TH

$(generateFakeField "music" "instruments")

$(generateFakeField "music" "bands")

$(generateFakeField "music" "albums")

$(generateFakeField "music" "genres")

-- | @since 1.0
$(generateFakeField "music" "mambo_no_5")



$(generateFakeFields "music" ["hiphop","subgenres"])

$(generateFakeFields "music" ["hiphop","groups"])

$(generateFakeFields "music" ["hiphop","artist"])


