{-# LANGUAGE ScopedTypeVariables #-}

module TextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import qualified Faker.Ancient as AN
import qualified Faker.App as AP
import qualified Faker.Appliance as AP
import qualified Faker.Artist as AR
import qualified Faker.Bank as BA
import qualified Faker.Beer as BE
import qualified Faker.Book.CultureSeries as CE

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

verifyFakes :: [Fake Text] -> IO [Bool]
verifyFakes funs = do
  let fs :: [IO Text] = map generate funs
      gs :: [IO Bool] = map (\f -> isText <$> f) fs
  sequence gs

spec :: Spec
spec = do
  describe "TextSpec" $ do
    it "Address" $ do
      let functions :: [Fake Text] =
            [ FA.country
            , FA.cityPrefix
            , FA.citySuffix
            , FA.countryCode
            , FA.countryCodeLong
            , FA.buildingNumber
            , FA.communityPrefix
            , FA.communitySuffix
            , FA.community
            , FA.streetSuffix
            , FA.secondaryAddress
            , FA.postcode
            , FA.state
            , FA.stateAbbr
            , FA.timeZone
            , FA.city
            , FA.streetName
            , FA.streetAddress
            , FA.fullAddress
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Ancient" $ do
      let functions :: [Fake Text] = [AN.god, AN.primordial, AN.hero, AN.titan]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "App" $ do
      let functions :: [Fake Text] = [AP.name, AP.version]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Appliance" $ do
      let functions :: [Fake Text] = [AP.brand, AP.equipment]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Artist" $ do
      let functions :: [Fake Text] = [AR.names]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Bank" $ do
      let functions :: [Fake Text] = [BA.name, BA.swiftBic]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Beer" $ do
      let functions :: [Fake Text] =
            [BE.name, BE.brand, BE.hop, BE.yeast, BE.malt, BE.style]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Book.CultureSeries" $ do
      let functions :: [Fake Text] =
            [ CE.books
            , CE.cultureShips
            , CE.cultureShipClasses
            , CE.cultureShipClassAbvs
            , CE.civs
            , CE.planets
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
