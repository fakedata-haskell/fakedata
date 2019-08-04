{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Markdown where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Markdown
import Faker.TH

$(generateFakeField "markdown" "headers")

$(generateFakeField "markdown" "emphasis")
