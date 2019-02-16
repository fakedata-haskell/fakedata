{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module CoffeeSpec where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Text hiding (all, map)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Faker
import Faker.Coffee
import Faker.Internal
import Faker.Provider.Coffee
import Test.Hspec

spec :: Spec
spec = do
  describe "Coffee" $ do
    it "Nested field" $ do
      ctries <- coffeeRegionsBrazilProvider defaultFakerSettings
      (V.toList ctries) `shouldBe` ["Sul Minas", "Mogiana", "Cerrado"]
    it "Nested field (via TH)" $ do
      ctries <- coffeeRegionsColombiaProvider defaultFakerSettings
      (ctries) `shouldSatisfy` (\x -> V.length x > 3)
    it "random region" $ do
      ctries <- generate regionsColombia
      ctries `shouldBe` "Santander"
    it "brazil region (via TH)" $ do
      ctries <- generate regionsBrazil
      ctries `shouldBe` "Cerrado"
    it "India region (via TH)" $ do
      ctries <- generate regionsIndia
      ctries `shouldBe` "Sheveroys"
