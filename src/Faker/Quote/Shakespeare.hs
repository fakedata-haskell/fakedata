{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Quote.Shakespeare where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Shakespeare
import Faker.TH

$(generateFakeField "shakespeare" "hamlet")

$(generateFakeField "shakespeare" "as_you_like_it")

$(generateFakeField "shakespeare" "king_richard_iii")

$(generateFakeField "shakespeare" "romeo_and_juliet")
