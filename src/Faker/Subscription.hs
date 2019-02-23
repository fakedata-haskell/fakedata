{-# LANGUAGE TemplateHaskell #-}

module Faker.Subscription where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Subscription
import Faker.TH


$(generateFakeField "subscription" "plans")

$(generateFakeField "subscription" "statuses")

$(generateFakeField "subscription" "payment_methods")

$(generateFakeField "subscription" "subscription_terms")

$(generateFakeField "subscription" "payment_terms")






