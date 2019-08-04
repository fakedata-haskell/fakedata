{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Job where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Job
import Faker.TH

field :: Fake Text
field = Fake (resolver jobField2Provider)

$(generateFakeField "job" "seniority")

$(generateFakeField "job" "position")

$(generateFakeField "job" "key_skills")

$(generateFakeField "job" "employment_type")

$(generateFakeField "job" "education_level")

$(generateFakeFieldUnresolved "job" "title")
