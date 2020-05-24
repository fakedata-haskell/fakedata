{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module EnUsTextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import Faker.Combinators (listOf)
import qualified Faker.Company as CO
import qualified Faker.Internet as IN
import qualified Faker.Name as NA
import qualified Faker.PhoneNumber as PH
import qualified Faker.Vehicle as VE

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

isTexts :: [Text] -> Bool
isTexts xs = and $ map isText xs

locale :: Text
locale = "en-US"

fakerSettings :: FakerSettings
fakerSettings = setLocale locale defaultFakerSettings

verifyDistributeFakes :: [Fake Text] -> IO [Bool]
verifyDistributeFakes funs = do
  let fs :: [IO [Text]] =
        map (generateWithSettings fakerSettings) $ map (listOf 100) funs
      gs :: [IO Bool] = map (\f -> isTexts <$> f) fs
  sequence gs

spec :: Spec
spec = do
  describe "TextSpec" $ do
    it "validates en-US locale" $ do
      let functions :: [Fake Text] =
            [ 
              IN.domainSuffix
            , FA.countryCode
            , FA.fullAddress
            , FA.buildingNumber
            , FA.postcode
            , FA.streetSuffix
            , FA.streetName
            , FA.stateAbbr
            , FA.cityPrefix
            , FA.citySuffix
            , FA.city
            , FA.secondaryAddress
            , FA.fullAddress
            , FA.streetAddress
            , PH.countryCode
            , PH.formats
            , PH.cellPhoneFormat
            , NA.firstName
            , NA.lastName
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
