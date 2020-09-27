{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module AddressSpec where

import qualified Data.Map as M
import Data.Text hiding (all, map)
import qualified Data.Text as T
import qualified Data.Vector as V
import Faker hiding (defaultFakerSettings)
import Faker.Address
import Test.Hspec
import TestImport
import Faker.Internal

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
      txt `shouldBe` "32-SZJX-4351"
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
        bno `shouldBe` "351"
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
    describe "Functions in address module" $ do
      it "Country" $ do
        val <- generateWithSettings defaultFakerSettings country
        val `shouldBe` "Ecuador"
      it "cityPrefix" $ do
        val <- generateWithSettings defaultFakerSettings cityPrefix
        val `shouldBe` "East"
      it "citySuffix" $ do
        val <- generateWithSettings defaultFakerSettings citySuffix
        val `shouldBe` "burgh"
      it "countryCode" $ do
        val <- generateWithSettings defaultFakerSettings countryCode
        val `shouldBe` "FJ"
      it "countryCodeLong" $ do
        val <- generateWithSettings defaultFakerSettings countryCodeLong
        val `shouldBe` "CYM"
      it "buildingNumber" $ do
        val <- generateWithSettings defaultFakerSettings buildingNumber
        val `shouldBe` "351"
      it "communityPrefix" $ do
        val <- generateWithSettings defaultFakerSettings communityPrefix
        val `shouldBe` "Pine"
      it "communitySuffix" $ do
        val <- generateWithSettings defaultFakerSettings communitySuffix
        val `shouldBe` "Oaks"
      it "community" $ do
        val <- generateWithSettings defaultFakerSettings community
        val `shouldBe` "Pine Place"
      it "streetSuffix" $ do
        val <- generateWithSettings defaultFakerSettings streetSuffix
        val `shouldBe` "Way"
      it "secondaryAddress" $ do
        val <- generateWithSettings defaultFakerSettings secondaryAddress
        val `shouldBe` "Suite 351"
      it "postcode" $ do
        val <- generateWithSettings defaultFakerSettings postcode
        val `shouldBe` "24351-4351"
      it "state" $ do
        val <- generateWithSettings defaultFakerSettings state
        val `shouldBe` "Michigan"
      it "stateAbbr" $ do
        val <- generateWithSettings defaultFakerSettings stateAbbr
        val `shouldBe` "MI"
      it "timeZone" $ do
        val <- generateWithSettings defaultFakerSettings timeZone
        val `shouldBe` "America/Chicago"
      it "city" $ do
        val <- generateWithSettings defaultFakerSettings city
        val `shouldBe` "East Vernita"
      it "streetName" $ do
        val <- generateWithSettings defaultFakerSettings streetName
        val `shouldBe` "Schmidt Ferry"
      it "streetAddress" $ do
        val <- generateWithSettings defaultFakerSettings streetAddress
        val `shouldBe` "351 Vernita Avenue"
      it "fullAddress" $ do
        val <- generateWithSettings defaultFakerSettings fullAddress
        val `shouldBe` "Suite 351 892 Donnelly Points, Kelleymouth, AK 66043-6043"
      it "mailBox" $ do
        val <- generateWithSettings defaultFakerSettings mailBox
        val `shouldBe` "PO Box 4351"
      it "cityWithState" $ do
        val <- generateWithSettings defaultFakerSettings cityWithState
        val `shouldBe` "East Vernita, Minnesota"

