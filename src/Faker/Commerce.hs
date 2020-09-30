{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Commerce where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Commerce
import Faker.TH

$(generateFakeField "commerce" "department")

$(generateFakeFields "commerce" ["product_name", "adjective"])

$(generateFakeFields "commerce" ["product_name", "material"])

$(generateFakeFields "commerce" ["product_name", "product"])

$(generateFakeFields "commerce" ["promotion_code", "adjective"])

$(generateFakeFields "commerce" ["promotion_code", "noun"])
