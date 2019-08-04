{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Source where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Source
import Faker.TH

$(generateFakeFields "source" ["hello_world", "ruby"])

$(generateFakeFields "source" ["hello_world", "javascript"])

$(generateFakeFields "source" ["print", "ruby"])

$(generateFakeFields "source" ["print", "javascript"])

$(generateFakeFields "source" ["print_1_to_10", "ruby"])

$(generateFakeFields "source" ["print_1_to_10", "javascript"])
