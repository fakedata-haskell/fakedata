{-# LANGUAGE TemplateHaskell #-}

module Faker.GreekPhilosophers where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.GreekPhilosophers
import Faker.TH


$(generateFakeField "greekPhilosophers" "names")

$(generateFakeField "greekPhilosophers" "quotes")






