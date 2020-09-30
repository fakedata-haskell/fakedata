{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Faker.Company where

import Data.Monoid ((<>))
import Data.Text
import qualified Data.Vector as V
import Faker
import Faker.Internal
import Faker.Provider.Company
import Faker.TH

$(generateFakeField "company" "suffix")

buzzword :: Fake Text
buzzword =
  Fake
    (\settings -> do
       vec :: V.Vector (V.Vector Text) <- companyBuzzwordsProvider settings
       let item :: IO (V.Vector Text) = rvec settings vec
       item' <- item
       rvec settings item')

bs :: Fake Text
bs =
  Fake
    (\settings -> do
       vec :: V.Vector (V.Vector Text) <- companyBsProvider settings
       let item :: V.Vector (IO Text) = V.map (\v -> rvec settings v) vec
           item' :: IO (V.Vector Text) = sequence item
       items <- item'
       let txt = V.foldl1' (\a b -> a <> " " <> b) items
       pure txt)

$(generateFakeFieldUnresolved "company" "name")

$(generateFakeField "company" "industry")

$(generateFakeField "company" "profession")

$(generateFakeField "company" "type")

-- | SIC code for classifying industries.
--
-- @since 0.2.0
--
$(generateFakeField "company" "sic_code")
