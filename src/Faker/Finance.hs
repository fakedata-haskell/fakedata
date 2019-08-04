{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Finance where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Finance
import Faker.TH

$(generateFakeFieldUnresolved "finance" "visa")

$(generateFakeFieldUnresolved "finance" "mastercard")

$(generateFakeFieldUnresolved "finance" "discover")

$(generateFakeFieldUnresolved "finance" "american_express")

$(generateFakeFieldUnresolved "finance" "diners_club")

$(generateFakeFieldUnresolved "finance" "jcb")

$(generateFakeFieldUnresolved "finance" "switch")

$(generateFakeFieldUnresolved "finance" "solo")

$(generateFakeFieldUnresolved "finance" "dankort")

$(generateFakeFieldUnresolved "finance" "maestro")

$(generateFakeFieldUnresolved "finance" "forbrugsforeningen")

$(generateFakeFieldUnresolved "finance" "laser")
