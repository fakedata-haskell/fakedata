{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Educator where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Educator
import Faker.TH

-- | @since 0.6.0
$(generateFakeField "educator" "school_name")

$(generateFakeField "educator" "secondary")

$(generateFakeFieldUnresolved "educator" "university")

$(generateFakeFieldUnresolved "educator" "secondary_school")

$(generateFakeFieldUnresolved "educator" "campus")

-- | @since 1.0
$(generateFakeFieldUnresolved "educator" "primary")

-- | @since 1.0
$(generateFakeFieldUnresolved "educator" "primary_school")

-- | @since 0.6.0
$(generateFakeField "educator" "subject")

-- | @since 0.6.0
$(generateFakeFieldUnresolved "educator" "degree")

-- | @since 0.6.0
$(generateFakeFieldUnresolved "educator" "course_name")

-- | @since 0.6.0
$(generateFakeFields "educator" ["tertiary", "university_type"])

$(generateFakeFields "educator" ["tertiary", "degree", "type"])

$(generateFakeFieldUnresolveds
    "educator"
    ["tertiary", "degree", "course_number"])
