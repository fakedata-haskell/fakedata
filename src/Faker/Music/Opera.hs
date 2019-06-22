{-# LANGUAGE TemplateHaskell #-}

module Faker.Music.Opera where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Opera
import Faker.TH

$(generateFakeFields "opera" ["italian", "by_giuseppe_verdi"])

$(generateFakeFields "opera" ["italian", "by_gioacchino_rossini"])

$(generateFakeFields "opera" ["italian", "by_gaetano_donizetti"])

$(generateFakeFields "opera" ["italian", "by_vincenzo_bellini"])
