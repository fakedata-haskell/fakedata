{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Source where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Source
import Faker.TH

$(generateFakeFields "source" ["hello_world", "ruby"])

$(generateFakeFields "source" ["hello_world", "javascript"])

-- | @since 1.0
$(generateFakeFields "source" ["hello_world", "c"])

-- | @since 1.0
$(generateFakeFields "source" ["hello_world", "php"])

-- | @since 1.0
$(generateFakeFields "source" ["hello_world", "python"])

-- | @since 1.0
$(generateFakeFields "source" ["hello_world", "java"])

-- | @since 1.0
$(generateFakeFields "source" ["hello_world", "elixir"])

$(generateFakeFields "source" ["print", "ruby"])

$(generateFakeFields "source" ["print", "javascript"])

-- | @since 1.0
$(generateFakeFields "source" ["print", "c"])

-- | @since 1.0
$(generateFakeFields "source" ["print", "php"])

-- | @since 1.0
$(generateFakeFields "source" ["print", "python"])

-- | @since 1.0
$(generateFakeFields "source" ["print", "java"])

-- | @since 1.0
$(generateFakeFields "source" ["print", "elixir"])

$(generateFakeFields "source" ["print_1_to_10", "ruby"])

$(generateFakeFields "source" ["print_1_to_10", "javascript"])

-- | @since 1.0
$(generateFakeFields "source" ["print_1_to_10", "c"])

-- | @since 1.0
$(generateFakeFields "source" ["print_1_to_10", "php"])

-- | @since 1.0
$(generateFakeFields "source" ["print_1_to_10", "python"])

-- | @since 1.0
$(generateFakeFields "source" ["print_1_to_10", "java"])

-- | @since 1.0
$(generateFakeFields "source" ["print_1_to_10", "elixir"])
