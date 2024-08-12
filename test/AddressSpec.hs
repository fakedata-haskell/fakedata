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
import Faker.Internal.Types (aesonKeyToText)

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
          (\s t -> pure $ aesonKeyToText t)
      txt `shouldBeOneOf` ["32-SZJX-4351", "32-GODI-8116"]
    it "doesn't get confused with garbage" $ do
      txt <-
        resolveUnresolved
          defaultFakerSettings
          (pure $ pure $ "abjakf-324jak")
          (\s t -> pure $ aesonKeyToText t)
      txt `shouldBe` "abjakf-324jak"
    describe "Address functions" $ do
      it "basic check" $ do
        fakeCountry <- generate country
        fakeCountry `shouldBeOneOf` ["Ecuador", "Macedonia"]
      it "Monad instance of Fake" $ do
        let someCountry :: Fake Text
            someCountry = do
              c1 <- country
              pure c1
        fakeCountry <- generate someCountry
        fakeCountry `shouldBeOneOf` ["Ecuador", "Macedonia"]
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
        fakeCountry `shouldBeOneOf` [("Ecuador", "Ecuador"), ("Macedonia","Macedonia")]
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
        bno `shouldBeOneOf` ["351", "116"]
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
        val `shouldBeOneOf` ["Ecuador", "Macedonia"]
      it "cityPrefix" $ do
        val <- generateWithSettings defaultFakerSettings cityPrefix
        val `shouldBeOneOf` ["East", "Port"]
      it "citySuffix" $ do
        val <- generateWithSettings defaultFakerSettings citySuffix
        val `shouldBeOneOf` ["burgh", "borough"]
      it "countryCode" $ do
        val <- generateWithSettings defaultFakerSettings countryCode
        val `shouldBeOneOf` ["FJ", "LB"]
      it "countryCodeLong" $ do
        val <- generateWithSettings defaultFakerSettings countryCodeLong
        val `shouldBeOneOf` ["CYM", "LBY"]
      it "buildingNumber" $ do
        val <- generateWithSettings defaultFakerSettings buildingNumber
        val `shouldBeOneOf` ["351", "116"]
      it "communityPrefix" $ do
        val <- generateWithSettings defaultFakerSettings communityPrefix
        val `shouldBeOneOf` ["Pine", "Royal"]
      it "communitySuffix" $ do
        val <- generateWithSettings defaultFakerSettings communitySuffix
        val `shouldBeOneOf` ["Oaks", "Gardens"]
      it "community" $ do
        val <- generateWithSettings defaultFakerSettings community
        val `shouldBeOneOf` ["Pine Place", "Royal Pointe"]
      it "streetSuffix" $ do
        val <- generateWithSettings defaultFakerSettings streetSuffix
        val `shouldBeOneOf` ["Way", "Parks"]
      it "secondaryAddress" $ do
        val <- generateWithSettings defaultFakerSettings secondaryAddress
        val `shouldBeOneOf` ["Suite 351", "Suite 116"]
      it "postcode" $ do
        val <- generateWithSettings defaultFakerSettings postcode
        val `shouldBeOneOf` ["24351-4351", "68116-8116"]
      it "state" $ do
        val <- generateWithSettings defaultFakerSettings state
        val `shouldBeOneOf` ["Michigan", "Rhode Island"]
      it "stateAbbr" $ do
        val <- generateWithSettings defaultFakerSettings stateAbbr
        val `shouldBeOneOf` ["MI", "RI"]
      it "timeZone" $ do
        val <- generateWithSettings defaultFakerSettings timeZone
        val `shouldBeOneOf` ["America/Chicago", "Australia/Brisbane"]
      it "city" $ do
        val <- generateWithSettings defaultFakerSettings city
        val `shouldBeOneOf` ["East Vernita", "Goldnerton"]
      it "streetName" $ do
        val <- generateWithSettings defaultFakerSettings streetName
        val `shouldBeOneOf` ["Schmidt Ferry", "Goldner Track"]
      it "streetAddress" $ do
        val <- generateWithSettings defaultFakerSettings streetAddress
        val `shouldBeOneOf` ["351 Vernita Avenue", "116 Will Manor"]
      it "fullAddress" $ do
        val <- generateWithSettings defaultFakerSettings fullAddress
        val `shouldBeOneOf`
          [ "Suite 351 892 Donnelly Points, Kelleymouth, AK 66043-6043"
          , "Suite 116 663 Russel Locks, Assuntamouth, UT 86075-6075"
          ]
      it "mailBox" $ do
        val <- generateWithSettings defaultFakerSettings mailBox
        val `shouldBeOneOf` ["PO Box 4351", "PO Box 8116"]
      it "cityWithState" $ do
        val <- generateWithSettings defaultFakerSettings cityWithState
        val `shouldBeOneOf` ["East Vernita, Minnesota", "Goldnerton, Arkansas"]
