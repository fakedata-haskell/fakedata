{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Quote.JackHandey where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.JackHandey
import Faker.TH

$(generateFakeField "jackHandey" "jack_handey")
