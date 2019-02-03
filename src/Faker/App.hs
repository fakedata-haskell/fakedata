module Faker.App where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.App

name :: Fake Text
name = Fake $ resolver nameAppProvider

author :: Fake Text
author = Fake $ unresolvedResolver authorAppProvider resolveAppText

version :: Fake Text
version = Fake $ unresolvedResolver versionAppProvider resolveAppText
