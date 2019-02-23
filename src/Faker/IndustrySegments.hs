{-# LANGUAGE TemplateHaskell #-}

module Faker.IndustrySegments where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.IndustrySegments
import Faker.TH


$(generateFakeField "industrySegments" "industry")

$(generateFakeField "industrySegments" "super_sector")

$(generateFakeField "industrySegments" "sector")

$(generateFakeField "industrySegments" "sub_sector")






