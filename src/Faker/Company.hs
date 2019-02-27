{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Faker.Company where

import Data.Text
import qualified Data.Vector as V
import Faker
import Faker.Internal
import Faker.Provider.Company
import Faker.TH

$(generateFakeField "company" "suffix")

$(generateFakeField "company" "buzzwords")

bs :: Fake Text
bs =
  Fake
    (\settings -> do
       vec <- companyBsProvider settings
       let item :: V.Vector (IO Text) = V.map (\v -> rvec settings v) vec
           item' :: IO (V.Vector Text) = sequence item
       items <- item'
       let txt = V.foldl1' (\a b -> a <> " " <> b) items
       pure txt)

-- $(generateFakeField "company" "bs")
$(generateFakeFieldUnresolved "company" "name")

$(generateFakeField "company" "industry")

$(generateFakeField "company" "profession")

$(generateFakeField "company" "type")
