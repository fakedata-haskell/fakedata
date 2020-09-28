{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Music.Prince where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Prince
import Faker.TH

$(generateFakeField "prince" "lyric")

$(generateFakeField "prince" "song")

$(generateFakeField "prince" "album")

$(generateFakeField "prince" "band")




