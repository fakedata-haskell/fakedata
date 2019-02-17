{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module FakerSpec where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Text hiding (all, map)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Faker
import Faker.Combinators
import Faker.Compass
import Faker.Educator
import Faker.Internal
import Faker.Provider.Educator
import Test.Hspec

spec :: Spec
spec = do
  describe "Faker Generate" $ do
    it "Nested unresolved field" $ do
      ctries <- generate tertiaryCourse_number
      (ctries) `shouldBe` "215"
  describe "Faker Compass Generate" $ do
    it "Resolver check" $ do
      ctries <- generate direction
      (ctries) `shouldBe` "SE"
    it "Resolver check" $ do
      ctries <- generate $ listOf 5 direction
      (ctries) `shouldSatisfy` (\x -> Prelude.length x == 5)
