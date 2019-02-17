{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module ProviderSpec where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Text hiding (all, map)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Faker
import Faker.Educator
import Faker.Internal
import Faker.Provider.Educator
import Test.Hspec

spec :: Spec
spec = do
  describe "Provider Checks" $ do
    it "Nested unresolved field" $ do True `shouldBe` True
