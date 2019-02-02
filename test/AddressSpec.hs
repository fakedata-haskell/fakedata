{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module AddressSpec where

import Config
import qualified Data.Map as M
import Data.Text hiding (all, map)
import qualified Data.Text as T
import qualified Data.Vector as V
import Faker
import Faker.Address
import Test.Hspec

fakerException :: Selector FakerException
fakerException = const True

spec :: Spec
spec = do
  describe "Address" $ do
    it "parses countries for defaultSettings" $ do
      ctries <- countriesProvider defaultFakerSettings
      ctries `shouldSatisfy` (\x -> V.length x >= 40)
    it "parses countryByCode for defaultSettings" $ do
      ctries <- countryByCodeProvider defaultFakerSettings
      ctries `shouldSatisfy` (\x -> Prelude.length (M.toList x) >= 40)
    it "parses countries for other locales" $ do
      locales <- populateLocales
      let settings :: [FakerSettings] =
            map (\l -> setLocale l defaultFakerSettings) locales
          ctries :: IO [V.Vector Text] = mapM countriesProvider settings
      ctries' :: [V.Vector Text] <- ctries
      let exp :: [Bool] = map (\x -> V.length x >= 10) ctries'
      True `shouldBe` (all (\x -> x == True) exp)
    it "produces random integer text" $ do
      let txt = interpolateNumbers defaultFakerSettings "#####"
      let num :: Int = read (unpack txt)
      num `shouldSatisfy` (\x -> x >= 0 && T.length txt == 5)
    it "produces random integer only for hash" $ do
      let txt = interpolateNumbers defaultFakerSettings "ab-##"
      txt `shouldSatisfy` (\x -> T.length x == 5 && (T.take 3 x == "ab-"))
    it "resolve field" $ do
      let field = resolveFields "#{community_prefix} #{community_suffix}"
      field `shouldBe` ["community_prefix", "community_suffix"]
    it "resolve field with dot" $ do
      let field =
            resolveFields "#{city_prefix} #{Name.first_name}#{city_suffix}"
      field `shouldBe` ["city_prefix", "Name.first_name", "city_suffix"]
    it "resolve field with dot (2)" $ do
      let field = resolveFields "#{Name.last_name} #{street_suffix}"
      field `shouldBe` ["Name.last_name", "street_suffix"]
    it "resolve field with commas" $ do
      let field =
            resolveFields
              "#{secondary_address} #{street_address}, #{city}, #{state_abbr} #{zip_code}"
      field `shouldBe`
        [ "secondary_address"
        , "street_address"
        , "city"
        , "state_abbr"
        , "zip_code"
        ]
    it "resolveAddressField" $ do
      item <- resolveAddressField defaultFakerSettings "community_suffix"
      item `shouldSatisfy` (\x -> T.length x > 0)
    it "uncons2" $ do
      let item = uncons2 "hello"
      item `shouldBe` Just ("he" :: String, "llo" :: Text)
    describe "operateField" $ do
      it "sample example" $ do
        let item = operateField "#{hello} #{world}" "jam"
        item `shouldBe` "jam #{world}"
      it "leading space example" $ do
        let item = operateField " #{hello} #{world}" "jam"
        item `shouldBe` " jam #{world}"
      it "trailing chars" $ do
        let item = operateField " #{hello} #{world} kool" "jam"
        item `shouldBe` " jam #{world} kool"
      it "edge case" $ do
        let item = operateField "this is" "jam"
        item `shouldBe` "this is"
    describe "operateFields" $ do
      it "sample example" $ do
        let item = operateFields "#{hello} #{world}" ["jam", "cream"]
        item `shouldBe` "jam cream"
      it "leading space example" $ do
        let item = operateFields " #{hello} #{world}" ["jam", "cream"]
        item `shouldBe` " jam cream"
      it "extra arguments" $ do
        let item =
              operateFields " #{hello} #{world} kool" ["jam", "kool", "name"]
        item `shouldBe` " jam kool kool"
    it "resolveAddressText" $ do
      item <-
        resolveAddressText
          defaultFakerSettings
          "#{community_prefix} #{community_suffix}"
      item `shouldSatisfy` (\x -> T.length x > 0 && T.any (== ' ') item)
    describe "Resolver check" $ do
      it "community" $ do
        item <-
          resolveAddressText
            defaultFakerSettings
            "#{community_prefix} #{community_suffix}"
        item `shouldSatisfy` (\x -> T.length x >= 5)
      it "community via function" $ do
        comm <- communityProvider defaultFakerSettings
        item <- resolveUnresolved defaultFakerSettings comm resolveAddressText
        item `shouldSatisfy` (\x -> T.length x >= 5)
      it "building_number" $ do
        comm <- buildingNumberProvider defaultFakerSettings
        item <- resolveUnresolved defaultFakerSettings comm resolveAddressText
        item `shouldSatisfy` (\x -> T.length x >= 3)
      it "secondary_address" $ do
        comm <- secondaryAddressProvider defaultFakerSettings
        item <- resolveUnresolved defaultFakerSettings comm resolveAddressText
        item `shouldSatisfy` (\x -> T.length x >= 5)
      it "street_address" $ do
        item <-
          resolveAddressText
            defaultFakerSettings
            "#{Name.last_name} #{street_suffix}"
        item `shouldSatisfy` (\x -> T.length x >= 5)
      it "street_address via function" $ do
        comm <- streetAddressProvider defaultFakerSettings
        item <- resolveUnresolved defaultFakerSettings comm resolveAddressText
        item `shouldSatisfy` (\x -> T.length x >= 5)
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
        fakeCountry `shouldBe` ("Ecuador", "French Guiana")
      it "Non equality of sequence" $ do
        let someCountry :: Fake (Text, Text)
            someCountry = do
              c1 <- country
              c2 <- country
              pure (c1, c2)
        (c1, c2) <- generate someCountry
        c1 `shouldNotBe` c2
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
        c1 `shouldNotBe` c2
    describe "Empty data sources" $ do
      it "For ee locale, countries is empty" $ do
        ctries <- countriesProvider (setLocale "ee" defaultFakerSettings)
        ctries `shouldSatisfy` (\x -> V.length x == 0)
      it "For ee locale, throws exception" $ do
        let action =
              generateWithSettings (setLocale "ee" defaultFakerSettings) country
        action `shouldThrow` fakerException
