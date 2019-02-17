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
import qualified Faker.ElderScrolls as ES
import Faker.ElectricalComponents
import Faker.Esport
import Faker.Fallout
import Faker.Internal
import Faker.Provider.Educator
import Test.Hspec

isText :: Text -> Bool
isText x = T.length x >= 1

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
    it "Elder Scroll" $ do
      ctries <- generate ES.first_name
      (ctries) `shouldSatisfy` isText
    it "Electrical component" $ do
      ctries <- generate passive
      (ctries) `shouldSatisfy` isText
    it "Esport" $ do
      ctries <- generate leagues
      (ctries) `shouldSatisfy` isText
    it "Fallout" $ do
      ctries <- generate factions
      ctries `shouldSatisfy` isText
