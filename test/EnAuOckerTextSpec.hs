{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module EnAuOckerTextSpec where

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
locale = "en-au-ocker"          

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
    it "validates en-au-ocker locale" $ do
      let functions :: [Fake Text] =
            [ 
              NA.lastName
            , NA.firstName

            , IN.domainSuffix

            , CO.suffix
              
            , PH.formats
            , PH.cellPhoneFormat
              
            , FA.postcode
            , FA.city
            , FA.stateAbbr
            , FA.streetName
            , FA.state
            , FA.buildingNumber
            , FA.streetSuffix
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
