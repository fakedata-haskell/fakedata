{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @since 1.0
module Faker.Tea where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Tea
import Faker.TH

$(generateFakeField "tea" "type")



$(generateFakeFields "tea" ["variety","black"])

$(generateFakeFields "tea" ["variety","oolong"])

$(generateFakeFields "tea" ["variety","green"])

$(generateFakeFields "tea" ["variety","white"])

$(generateFakeFields "tea" ["variety","herbal"])
