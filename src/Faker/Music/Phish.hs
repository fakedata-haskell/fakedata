{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Music.Phish where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Phish
import Faker.TH

$(generateFakeField "phish" "songs")

-- | @since 0.7.0
$(generateFakeField "phish" "albums")

-- | @since 0.7.0
$(generateFakeField "phish" "musicians")
