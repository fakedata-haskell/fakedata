{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module AddressSpec where

import qualified Data.Map as M
import Data.Text hiding (all, map)
import qualified Data.Text as T
import qualified Data.Vector as V
import Faker hiding (defaultFakerSettings)
import Faker.Address
import Faker.Internal
-- import Faker.Provider.Address
import Test.Hspec
import TestImport

fakerException :: Selector FakerException
fakerException = const True

spec :: Spec
spec = do
  describe "Address" $ do
    it "preserves unwanted things and works with numbers" $ do
      txt <-
        resolveUnresolved
          defaultFakerSettings
          (pure $ pure $ "32-????-####")
          (\s t -> pure t)
      txt `shouldBe` "32-XJZS-1534"
    it "doesn't get confused with garbage" $ do
      txt <-
        resolveUnresolved
          defaultFakerSettings
          (pure $ pure $ "abjakf-324jak")
          (\s t -> pure t)
      txt `shouldBe` "abjakf-324jak"
    describe "Address functions" $ do
      it "basic check" $ do
        fakeCountry <- generate country
        fakeCountry `shouldBe` "Ecuador"
      it "Monad instance of Fake" $ do
        let someCountry :: Fake Text
            someCountry = do
              c1 <- country
              pure c1
        fakeCountry <- generate someCountry
        fakeCountry `shouldBe` "Ecuador"
      it "Equality of normal generation and Monad" $ do
        fakeCountry <- generate country
        let someCountry :: Fake Text
            someCountry = do
              c1 <- country
              pure c1
        c2 <- generate someCountry
        fakeCountry `shouldBe` c2
      it "Monad instance of Fake (tuple)" $ do
        let someCountry :: Fake (Text, Text)
            someCountry = do
              c1 <- country
              c2 <- country
              pure (c1, c2)
        fakeCountry <- generate someCountry
        fakeCountry `shouldBe` ("Ecuador", "Ecuador")
      it "Monad instance of Fake (tuple)" $ do
        let someCountry :: Fake (Text, Text)
            someCountry = do
              c1 <- country
              c2 <- country
              pure (c1, c2)
        fakeCountry <- generateWithSettings (setNonDeterministic defaultFakerSettings) someCountry
        (fst fakeCountry) `shouldNotBe` (snd fakeCountry)
      it "Equality of sequence" $ do
        let someCountry :: Fake (Text, Text)
            someCountry = do
              c1 <- country
              c2 <- country
              pure (c1, c2)
        (c1, c2) <- generate someCountry
        c1 `shouldBe` c2
      it "Resolver based function" $ do
        bno <- generate buildingNumber
        bno `shouldBe` "153"
      it "Resolver fullAddress" $ do
        bno <- generate fullAddress
        bno `shouldSatisfy` (\x -> T.length x > 25)
      it "Resolver based function - monad" $ do
        let someBuilding :: Fake (Text, Text)
            someBuilding = do
              c1 <- buildingNumber
              c2 <- buildingNumber
              pure (c1, c2)
        (c1, c2) <- generate someBuilding
        c1 `shouldBe` c2
    describe "Empty data sources" $ do
      it "For ee locale, throws exception" $ do
        let action =
              generateWithSettings (setLocale "ee" defaultFakerSettings) country
        action `shouldThrow` fakerException
