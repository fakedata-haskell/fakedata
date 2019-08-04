{-# LANGUAGE ScopedTypeVariables #-}

module TextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import qualified Faker.Ancient as AN
import qualified Faker.App as AP
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
