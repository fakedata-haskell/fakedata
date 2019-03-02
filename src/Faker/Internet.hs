{-# LANGUAGE TemplateHaskell #-}

module Faker.Internet where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Internet
import Faker.TH

$(generateFakeField "internet" "free_email")

$(generateFakeField "internet" "domain_suffix")

$(generateFakeFields "internet" ["user_agent", "aol"])

$(generateFakeFields "internet" ["user_agent", "chrome"])

$(generateFakeFields "internet" ["user_agent", "firefox"])

$(generateFakeFields "internet" ["user_agent", "internet_explorer"])

$(generateFakeFields "internet" ["user_agent", "netscape"])

$(generateFakeFields "internet" ["user_agent", "opera"])

$(generateFakeFields "internet" ["user_agent", "safari"])
