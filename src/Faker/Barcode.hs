{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Barcode where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Barcode
import Faker.TH

$(generateFakeFieldSingleUnresolved "barcode" "ean_8")

$(generateFakeFieldSingleUnresolved "barcode" "ean_13")

$(generateFakeFieldSingleUnresolved "barcode" "upc_a")

$(generateFakeFieldUnresolved "barcode" "upc_e")

$(generateFakeFieldUnresolved "barcode" "composite_symbol")

$(generateFakeFieldUnresolved "barcode" "isbn")

$(generateFakeFieldSingleUnresolved "barcode" "ismn")

$(generateFakeFieldSingleUnresolved "barcode" "issn")


