{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.NatoPhoneticAlphabet where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.NatoPhoneticAlphabet
import Faker.TH

$(generateFakeField "natoPhoneticAlphabet" "code_word")
