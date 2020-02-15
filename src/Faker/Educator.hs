{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Educator where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Educator
import Faker.TH

$(generateFakeField "educator" "school_name")

$(generateFakeField "educator" "secondary")

$(generateFakeFieldUnresolved "educator" "university")

$(generateFakeFieldUnresolved "educator" "secondary_school")

$(generateFakeFieldUnresolved "educator" "campus")

$(generateFakeField "educator" "subject")

$(generateFakeFieldUnresolved "educator" "degree")

$(generateFakeFieldUnresolved "educator" "course_name")

$(generateFakeFields "educator" ["tertiary", "university_type"])

$(generateFakeFields "educator" ["tertiary", "degree", "type"])

$(generateFakeFieldUnresolveds "educator" ["tertiary", "degree", "course_number"])
